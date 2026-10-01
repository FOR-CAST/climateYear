test_that("getClimate keeps only the current year's file and returns the same layers", {
  skip_if_not_installed("SpaDES.core")
  skip_if_not_installed("terra")
  library(SpaDES.core)

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

  ## the same year sampled every time, and years that differ (a single-value range is avoided: `sample()` of one number)
  for (params in list(list(samplingRange = c(2003, 2003)), list(samplingRange = 2001:2005))) {
    unlink(list.files(climDir, pattern = "^year", full.names = TRUE))
    set.seed(1)
    sim <- simInit(
      times = list(start = 2001, end = 2004), modules = "climateYear",
      params = list(climateYear = c(params, list(.useCache = FALSE))),
      objects = list(projectedClimateRasters = proj),
      paths = list(modulePath = mp, inputPath = file.path(mp, "in"), outputPath = file.path(mp, "out"))
    )
    sim <- spades(sim, debug = FALSE)

    expect_lte(length(list.files(climDir, pattern = "^year.*\\.tif$")), 1L)

    yr <- paste0("year", sim$climateYear)
    expected <- terra::rast(lapply(proj, "[[", yr))
    expect_equal(terra::values(sim$currentClimateRasters), terra::values(expected), tolerance = 0)
    expect_identical(names(sim$currentClimateRasters), names(expected))
    expect_equal(nrow(sim$climateYearRecord), 4L)
  }
})
