defineModule(sim, list(
  name = "climateYear",
  description = paste("this is a very simplistic module designed to give users more control over how climate layers", 
                      "are sampled and supplied in simulations concerned with some form of NRV, ie where the simulation",
                      "length necessitates sampling climate layers instead of writing them to disk and annually retrieving them.", 
                      "It provides a measure of control over the sampling protocol used to select a given year, and ensures",
                      "consistent use of years across multiple modules when sampling is involved. Based on time(sim) and",
                      "user parameters, it determines whether to select from historical or projected climate rasters when",
                      "building currentClimateRasters"),
  keywords = c(),
  authors = c(person(c("Ian", "Eddy", role = c("aut", "cre"), email = "ian.eddy@nrcan-rncan.gc.ca"))),
  childModules = character(0),
  version = list(climateYear = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  loadOrder = list(before = c("fireSense_dataPrepPredict")),
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "climateYear.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 2.1.8.9999)", "ggplot2", "sf", "terra", "data.table"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                    "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"), 
    defineParameter("samplingEndYear", "numeric", end(sim), NA, NA, 
                    desc = paste("if randomly sampling the year for a given climate,", 
                                 "the simulation year at which to end this process, if applicable")),
    defineParameter("samplingRange", "numeric", NA, NA, NA, 
                    "Vector giving years from which to sample. The default is all years ",
                    " in sort(unique(c(historicalClimateRaster, projectedClimateRasters)))"),
    defineParameter("samplingStartYear", "numeric", NA, NA, NA, 
                    desc = paste("if randomly sampling the year for a given climate,", 
                                 "the simulation year at which to begin this process"))
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "historicalClimateRasters", objectClass = "list", 
                 desc = paste("optional list of SpatRasters, with layers corresponding to years.",
                              "Each list element should be a different variable with corresponding names.",
                              "Each layer should be named following the convention 'year<year>`, e.g. year2009.",
                              "The object is used solely to determine the available years from which to sample")),
    expectsInput(objectName = "projectedClimateRasters", objectClass = "list", 
                 desc = paste("optional list of SpatRasters, with layers corresponding to years.",
                              "Each list element should be a different variable with corresponding names.",
                              "Each layer should be named following the convention 'year<year>`, e.g. year2009.",
                              "The object is used solely to determine the available years from which to sample"))
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "climateYear", objectClass = "numeric", 
                  desc = "a year from projectedClimateRasters, updated annually"),
    createsOutput(objectName = "climateYearRecord", objectClass = "data.table", 
                  desc = "record of which climate year was used for which simulation year"),
    createsOutput(objectName = "climateYearsUsed", objectClass = "data.table",
                  desc = paste("one row per simulation year and climate variable: the climate year used,",
                               "whether it came from historical or projected rasters, the source file (`sourceFile`),",
                               "its md5 checksum (`sourceMd5`, taken the first time the file is used in a run)",
                               "and layer name it was read from, and the per-year file (`yearFile`, a .vrt",
                               "in `file.path(outputPath(sim), \"climate\")` that points at the source band).",
                               "Also written to `climateYearsUsed.csv` in `outputPath(sim)` every year")),
    createsOutput(objectName = "currentClimateRasters", objectClass = "SpatRaster", 
                  desc= "a single-year subset of projected or historical rasters")
  )
))

doEvent.climateYear = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
    
      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "climateYear", "getClimate")
    },
    getClimate = {
      availableYears <- c()
      if (!is.null(sim$projectedClimateRasters)){
        availableYears <- names(sim$projectedClimateRasters[[1]])
      }
      if (!is.null(sim$historicalClimateRasters)){
        availableYears <- sort(unique(c(availableYears, names(sim$historicalClimateRasters[[1]]))))
      }
     
      sim$climateYear <- sampleYear(Time = time(sim), 
                                    Available = availableYears,
                                    Starting = P(sim)$samplingStartYear,
                                    Ending = P(sim)$samplingEndYear,
                                    Range = P(sim)$samplingRange)
      
      #prioritize historical rasters
      rasToGet <- paste0("year", sim$climateYear)
      if (any(rasToGet %in% names(sim$historicalClimateRasters[[1]]))) {
        srcRasters <- sim$historicalClimateRasters
        srcType <- "historical"
      } else {
        srcRasters <- sim$projectedClimateRasters
        srcType <- "projected"
      }
      currentLyrs <- lapply(srcRasters, "[[", rasToGet) |> rast()
      srcFiles <- vapply(srcRasters, function(x) paste(unique(Filenames(x)), collapse = ";"), character(1))
      
      ## Each simulation year gets its own small .vrt in the run's outputPath that points at the
      ## source stack's band (no pixels are copied). Sources that are not single files on disk
      ## are written to a .tif instead.
      climDir <- file.path(outputPath(sim), "climate")
      dir.create(climDir, recursive = TRUE, showWarnings = FALSE)
      fnStem <- file.path(climDir, paste0("climate_simYear", time(sim), "_climYear", sim$climateYear))
      fn <- paste0(fnStem, ".vrt")
      if (buildYearVrt(srcRasters, rasToGet, names(currentLyrs), fn)) {
        sim$currentClimateRasters <- rast(fn)
        names(sim$currentClimateRasters) <- names(currentLyrs)
      } else {
        fn <- paste0(fnStem, ".tif")
        sim$currentClimateRasters <- writeRaster(currentLyrs, filename = fn, overwrite = TRUE)
      }
      
      ## md5 of a source stack can be multi-GB: compute it once per file per run
      srcMd5 <- vapply(srcFiles, function(srcF) {
        files <- strsplit(srcF, ";", fixed = TRUE)[[1]]
        files <- files[nzchar(files) & file.exists(files)]
        if (!length(files)) return(NA_character_)
        newF <- setdiff(files, names(mod$md5))
        if (length(newF)) mod$md5[newF] <- unname(tools::md5sum(newF))
        paste(mod$md5[files], collapse = ";")
      }, character(1))
      
      ## one row per climate variable, so it is known later which climate each simulation year used
      sim$climateYearsUsed <- rbind(sim$climateYearsUsed,
                                    data.table(simYear = time(sim),
                                               climateYear = sim$climateYear,
                                               source = srcType,
                                               variable = if (is.null(names(srcRasters))) as.character(seq_along(srcRasters)) else names(srcRasters),
                                               sourceFile = unname(srcFiles),
                                               sourceMd5 = unname(srcMd5),
                                               layer = rasToGet,
                                               yearFile = normalizePath(fn, mustWork = FALSE)))
      
      ## The whole table is rewritten every year (to a temporary file, then renamed), so a killed
      ## run leaves a complete CSV of the finished years; appending could leave a partial last line
      ## and would duplicate rows if a run restarts into the same outputPath.
      csv <- file.path(outputPath(sim), "climateYearsUsed.csv")
      csvTmp <- paste0(csv, ".tmp")
      data.table::fwrite(sim$climateYearsUsed, csvTmp)
      file.rename(csvTmp, csv)
      
      sim$climateYearRecord <- rbind(sim$climateYearRecord, 
                                     data.table(simYear = time(sim), 
                                                climateYear = sim$climateYear))
      
      sim <- scheduleEvent(sim, time(sim) + 1, "climateYear", "getClimate")
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

### template initialization
Init <- function(sim) {
 mod$md5 <- character(0) ## md5 of each source stack file, filled as files are first used

 #make climateYearRecord
 sim$climateYearRecord <- data.table(simYear = numeric(0), climateYear = numeric(0)) 
 sim$climateYearsUsed <- data.table(simYear = numeric(0), climateYear = numeric(0),
                                    source = character(0), variable = character(0),
                                    sourceFile = character(0), sourceMd5 = character(0),
                                    layer = character(0),
                                    yearFile = character(0))
 
 return(invisible(sim))
}
### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

## Writes a .vrt at `vrtFile` with one band per variable: band `layerName` of each variable's
## source stack, with its layer name (`lyrNames`) as band description. Source paths are absolute,
## so the .vrt stays valid if it is copied. Returns FALSE (and writes nothing) if a variable is
## not a single file on disk, lacks the layer, or the grids differ.
buildYearVrt <- function(srcRasters, layerName, lyrNames, vrtFile) {
  files <- vapply(srcRasters, function(x) {
    f <- unique(Filenames(x))
    if (length(f) == 1L && nzchar(f) && file.exists(f)) normalizePath(f) else NA_character_
  }, character(1))
  bands <- vapply(srcRasters, function(x) match(layerName, names(x)), integer(1))
  if (anyNA(files) || anyNA(bands)) return(FALSE)
  
  parts <- lapply(seq_along(files), function(i) {
    tmp <- tempfile(fileext = ".vrt")
    on.exit(unlink(tmp))
    sf::gdal_utils("buildvrt", files[i], tmp, options = c("-b", bands[i]), quiet = TRUE)
    x <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
    bandStart <- regexpr("<VRTRasterBand", x, fixed = TRUE)
    list(header = substr(x, 1L, bandStart - 1L),
         band = sub("\\s*</VRTDataset>\\s*$", "", substring(x, bandStart)))
  })
  if (length(unique(vapply(parts, function(p) p$header, character(1)))) != 1L) return(FALSE)
  
  esc <- function(z) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", z, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  bandsXml <- vapply(seq_along(parts), function(i) {
    b <- parts[[i]]$band
    b <- sub("band=\"[0-9]+\"", paste0("band=\"", i, "\""), b)
    b <- sub("(<VRTRasterBand[^>]*>)", paste0("\\1\n    <Description>", esc(lyrNames[i]), "</Description>"), b)
    sub("<SourceFilename[^>]*>[^<]*</SourceFilename>",
        paste0("<SourceFilename relativeToVRT=\"0\">", esc(files[i]), "</SourceFilename>"), b)
  }, character(1))
  writeLines(paste0(parts[[1]]$header, paste(bandsXml, collapse = "\n"), "\n</VRTDataset>"), vrtFile)
  TRUE
}

sampleYear <- function(Range, Starting, Ending, Time, Available) {
  
  if (is.na(Ending)) {
    Ending <- Time + 1 # protect in future `if` statements
  }
  
  Available <- na.omit(as.numeric(gsub("[^0-9]", "", Available)))
  #na.omit to account for projected normals
  if (any(is.na(Range))) {
    Range <- Available
  } else {
    Range <- Range[Range %in% Available]
  }
  
  if (Time %in% Range) {
    theYear <- Time
  } else {
    theYear <- Range[sample.int(length(Range), 1L)] ## `sample(Range)` samples 1:Range if length 1
  }
  
  
  
  # if (any(!is.na(Starting))) {
  #   if (Starting <= Time & Time <= Ending) {
  #     theYear <- sample(Range, size = 1)
  #   # This next was Time %in% Available, but Range is what needs to be respected not Available
  #     # Eliot changed March 17, 2026
  #   } else if (Time %in% Range) {
  #     #sample, but not yet
  #     theYear <- Time
  #   } else { # if (all(Range %in% Available)) {
  #     # The Range above is already reduced to whatever is Available; so no stop needed
  #     theYear <- sample(Range, size = 1)
  #   # } else {
  #   #   #sample, but not yet and the current year is not in the available years...
  #   #   stop("climateYear does not have any available years?")
  #   }
  # } else if (Time %in% Range) {
  #   theYear <- Time
  # } else {
  #   #do not explicit sample but no available years, so grab anything
  #   theYear <- sample(Range, size = 1)
  # }
  
  return(theYear)
}

.inputObjects <- function(sim) {
 
  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("projectedClimateRasters", sim)) {
    Range <- start(sim):end(sim)
    sim$projectedClimateRasters <- list("fooVar" = terra::rast(nlyrs = Range))
    names(sim$projectedClimateRasters[[1]]) <- paste0("year",Range)
  }
  
  return(invisible(sim))
}
