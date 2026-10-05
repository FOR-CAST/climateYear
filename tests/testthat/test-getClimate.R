## a module function, without running defineModule()
modFun <- function(name) {
  exprs <- parse(file.path(testthat::test_path(), "..", "..", "climateYear.R"))
  isFn <- vapply(exprs, function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                   identical(e[[2]], as.name(name)), logical(1))
  env <- new.env()
  eval(exprs[isFn][[1]], env)
  get(name, env)
}

setupSim <- function(params, proj, mp) {
  set.seed(1)
  simInit(
    times = list(start = 2001, end = 2004), modules = "climateYear",
    params = list(climateYear = c(params, list(.useCache = FALSE))),
    objects = list(projectedClimateRasters = proj),
    paths = list(modulePath = mp, inputPath = file.path(mp, "in"), outputPath = file.path(mp, "out"))
  )
}

test_that("getClimate writes a small .vrt per year, keeps every year, and records the climate used", {
  skip_if_not_installed("SpaDES.core")
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  skip_if_not_installed("reproducible")
  library(SpaDES.core)
  library(data.table)
  buildYearVrt <- modFun("buildYearVrt")

  modDir <- normalizePath(file.path(testthat::test_path(), "..", ".."))
  mp <- withr::local_tempdir()
  file.copy(modDir, mp, recursive = TRUE)
  file.rename(file.path(mp, basename(modDir)), file.path(mp, "climateYear"))

  climDir <- file.path(mp, "climate")
  dir.create(climDir)
  ## the two variables hold their years in a different order, so the band index differs
  mk <- function(offset, yrs) {
    r <- terra::rast(nrows = 20, ncols = 20, nlyrs = 5, xmin = 0, xmax = 20, ymin = 0, ymax = 20,
                     crs = "EPSG:3857")
    terra::values(r) <- offset + seq_len(terra::ncell(r) * 5) / 7
    names(r) <- paste0("year", yrs)
    f <- file.path(climDir, paste0("var", offset, "_projected_x.tif"))
    terra::writeRaster(r, f, overwrite = TRUE)
    terra::rast(f)
  }
  proj <- list(CMD = mk(0, 2001:2005), MDC = mk(1000, 2005:2001))

  for (params in list(list(samplingRange = 2003), list(samplingRange = 2001:2005))) {
    unlink(file.path(mp, "out"), recursive = TRUE)
    sim <- spades(setupSim(params, proj, mp), debug = FALSE)

    used <- sim$climateYearsUsed
    expect_equal(nrow(used), 4L * length(proj)) ## 4 simulation years x 2 variables
    expect_identical(sort(unique(used$simYear)), as.numeric(2001:2004))
    expect_identical(unique(used$source), "projected")
    expect_setequal(used$variable, names(proj))
    rec <- sim$climateYearRecord
    expect_equal(unique(used[, c("simYear", "climateYear")])[order(simYear)]$climateYear,
                 rec[order(simYear)]$climateYear)
    expect_identical(used$layer, paste0("year", used$climateYear))
    expect_true(all(basename(used[variable == "CMD"]$sourceFile) == "var0_projected_x.tif"))
    if (length(params$samplingRange) == 1L) {
      expect_true(all(used$climateYear == params$samplingRange)) ## a single-year range
    }

    ## checksum of each source stack
    expect_identical(used$sourceMd5, unname(tools::md5sum(used$sourceFile)))

    ## one .vrt per simulation year in outputPath/climate, named by simulation and climate year
    outDir <- file.path(mp, "out")
    expect_true(all(grepl("\\.vrt$", used$yearFile)))
    expected <- file.path(normalizePath(outDir), "climate",
                          sprintf("climate_simYear%d_climYear%d.vrt", rec$simYear, rec$climateYear))
    expect_setequal(used$yearFile, expected)
    vrts <- list.files(file.path(outDir, "climate"), full.names = TRUE)
    expect_equal(length(vrts), 4L)
    expect_setequal(normalizePath(vrts), expected)
    expect_length(list.files(climDir, pattern = "^(year|climate_).*\\.(vrt|tif)$"), 0L)
    expect_true(all(file.size(vrts) < 5000)) ## a pointer, not a copy of the pixels

    ## the CSV in outputPath has every row of the table
    csv <- file.path(outDir, "climateYearsUsed.csv")
    expect_true(file.exists(csv))
    fromCsv <- data.table::fread(csv, colClasses = list(character = c("sourceMd5")))
    expect_identical(names(fromCsv), names(used))
    expect_equal(nrow(fromCsv), 4L * length(proj))
    expect_equal(as.data.frame(fromCsv), as.data.frame(used), tolerance = 0)

    ## a fresh R process recovers each year's climate from the CSV and .vrt alone
    skip_if_not_installed("callr")
    fresh <- callr::r(function(csv) {
      u <- data.table::fread(csv)
      lapply(split(u, u$simYear), function(d) {
        v <- terra::rast(unique(d$yearFile))
        list(climateYear = unique(d$climateYear), values = terra::values(v), names = names(v))
      })
    }, args = list(csv = csv))
    expect_length(fresh, 4L)
    for (k in names(fresh)) {
      lyrs <- terra::rast(lapply(proj, "[[", paste0("year", fresh[[k]]$climateYear)))
      expect_equal(fresh[[k]]$values, terra::values(lyrs), tolerance = 0)
      expect_identical(fresh[[k]]$names, names(proj))
    }

    ## each year's .vrt reads the same values, names, extent and crs as a copy of that year's layers
    digs <- vapply(unique(used$climateYear), function(cy) {
      lyrs <- terra::rast(lapply(proj, "[[", paste0("year", cy)))
      copy <- terra::writeRaster(lyrs, withr::local_tempfile(fileext = ".tif"))
      f <- used[climateYear == cy]$yearFile[1]
      v <- terra::rast(f)
      expect_equal(terra::values(v), terra::values(copy), tolerance = 0)
      expect_identical(names(v), names(copy)) ## names are stored in the .vrt
      expect_equal(as.vector(terra::ext(v)), as.vector(terra::ext(copy)))
      expect_true(terra::same.crs(v, copy))
      expect_identical(unique(terra::sources(v)), f) ## file-backed on the .vrt
      reproducible::.robustDigest(v)
    }, character(1))
    ## reproducible digests the .vrt (the band selection), so years differ; a rebuilt one is the same
    expect_equal(length(unique(digs)), length(digs))
    cy <- unique(used$climateYear)[1]
    again <- file.path(climDir, "again.vrt")
    expect_true(buildYearVrt(proj, paste0("year", cy), names(proj), again))
    expect_identical(reproducible::.robustDigest(terra::rast(again)), unname(digs[1]))

    ## the raster handed to other modules in the last year has that year's values
    yr <- paste0("year", sim$climateYear)
    expected <- terra::rast(lapply(proj, "[[", yr))
    expect_equal(terra::values(sim$currentClimateRasters), terra::values(expected), tolerance = 0)
    expect_identical(names(sim$currentClimateRasters), names(expected))
    expect_equal(nrow(rec), 4L)
  }
})

test_that("getClimate falls back to a .tif copy when the sources are not files", {
  skip_if_not_installed("SpaDES.core")
  skip_if_not_installed("terra")
  library(SpaDES.core)
  buildYearVrt <- modFun("buildYearVrt")
  modDir <- normalizePath(file.path(testthat::test_path(), "..", ".."))
  mp <- withr::local_tempdir()
  file.copy(modDir, mp, recursive = TRUE)
  file.rename(file.path(mp, basename(modDir)), file.path(mp, "climateYear"))
  r <- terra::rast(nrows = 5, ncols = 5, nlyrs = 5, xmin = 0, xmax = 5, ymin = 0, ymax = 5,
                   vals = seq_len(125))
  names(r) <- paste0("year", 2001:2005)
  proj <- list(CMD = r)
  ## in-memory sources have no file to point a .vrt at
  expect_false(buildYearVrt(proj, "year2002", "year2002", tempfile(fileext = ".vrt")))
})
