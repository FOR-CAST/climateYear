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
    unlink(list.files(climDir, pattern = "^year", full.names = TRUE))
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

    ## checksum of each source stack and the .vrt of each year
    expect_identical(used$sourceMd5, unname(tools::md5sum(used$sourceFile)))
    expect_true(all(grepl("\\.vrt$", used$yearFile)))
    yearFiles <- list.files(climDir, pattern = "^year.*\\.vrt$", full.names = TRUE)
    expect_equal(length(yearFiles), length(unique(used$climateYear)))
    expect_setequal(normalizePath(yearFiles), unique(used$yearFile))
    expect_length(list.files(climDir, pattern = "^year.*\\.tif$"), 0L)
    expect_true(all(file.size(yearFiles) < 5000)) ## a pointer, not a copy of the pixels

    ## each year's .vrt reads the same values, names, extent and crs as a copy of that year's layers
    digs <- vapply(unique(used$climateYear), function(cy) {
      lyrs <- terra::rast(lapply(proj, "[[", paste0("year", cy)))
      copy <- terra::writeRaster(lyrs, withr::local_tempfile(fileext = ".tif"))
      f <- unique(used[climateYear == cy]$yearFile)
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
