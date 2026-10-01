test_that("getClimate keeps every year's file and records the climate each year used", {
  skip_if_not_installed("SpaDES.core")
  skip_if_not_installed("terra")
  library(SpaDES.core)
  library(data.table)

  modDir <- normalizePath(file.path(testthat::test_path(), "..", ".."))
  mp <- withr::local_tempdir()
  file.copy(modDir, mp, recursive = TRUE)
  file.rename(file.path(mp, basename(modDir)), file.path(mp, "climateYear"))

  climDir <- file.path(mp, "climate")
  dir.create(climDir)
  mk <- function(offset) {
    r <- terra::rast(nrows = 20, ncols = 20, nlyrs = 5, xmin = 0, xmax = 20, ymin = 0, ymax = 20)
    terra::values(r) <- offset + seq_len(terra::ncell(r) * 5) / 7
    names(r) <- paste0("year", 2001:2005)
    f <- file.path(climDir, paste0("var", offset, "_projected_x.tif"))
    terra::writeRaster(r, f, overwrite = TRUE)
    terra::rast(f)
  }
  proj <- list(CMD = mk(0), MDC = mk(1000))

  for (params in list(list(samplingRange = 2003), list(samplingRange = 2001:2005))) {
    unlink(list.files(climDir, pattern = "^year", full.names = TRUE))
    set.seed(1)
    sim <- simInit(
      times = list(start = 2001, end = 2004), modules = "climateYear",
      params = list(climateYear = c(params, list(.useCache = FALSE))),
      objects = list(projectedClimateRasters = proj),
      paths = list(modulePath = mp, inputPath = file.path(mp, "in"), outputPath = file.path(mp, "out"))
    )
    sim <- spades(sim, debug = FALSE)

    used <- sim$climateYearsUsed
    expect_equal(nrow(used), 4L * length(proj)) ## 4 simulation years x 2 variables
    expect_identical(sort(unique(used$simYear)), as.numeric(2001:2004))
    expect_identical(unique(used$source), "projected")
    expect_setequal(used$variable, names(proj))
    ## the table agrees with climateYearRecord, and names the layer and source files
    rec <- sim$climateYearRecord
    expect_equal(unique(used[, c("simYear", "climateYear")])[order(simYear)]$climateYear,
                 rec[order(simYear)]$climateYear)
    expect_identical(used$layer, paste0("year", used$climateYear))
    expect_true(all(basename(used[variable == "CMD"]$sourceFile) == "var0_projected_x.tif"))
    if (length(params$samplingRange) == 1L) {
      expect_true(all(used$climateYear == params$samplingRange)) ## a single-year range
    }

    ## every year's file is kept: one per climate year used (a repeated year shares its file)
    yearFiles <- list.files(climDir, pattern = "^year.*\\.tif$", full.names = TRUE)
    expect_equal(length(yearFiles), length(unique(used$climateYear)))
    expect_setequal(normalizePath(yearFiles), unique(used$yearFile))
    ## and each holds that climate year's layers from the source stacks
    for (cy in unique(used$climateYear)) {
      f <- unique(used[climateYear == cy]$yearFile)
      expect_equal(terra::values(terra::rast(f)),
                   terra::values(terra::rast(lapply(proj, "[[", paste0("year", cy)))),
                   tolerance = 0)
    }

    ## layers returned in the last year are unchanged
    yr <- paste0("year", sim$climateYear)
    expected <- terra::rast(lapply(proj, "[[", yr))
    expect_equal(terra::values(sim$currentClimateRasters), terra::values(expected), tolerance = 0)
    expect_identical(names(sim$currentClimateRasters), names(expected))
    expect_equal(nrow(sim$climateYearRecord), 4L)
  }
})
