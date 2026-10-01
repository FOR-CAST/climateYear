test_that("sampleYear returns the only year of a length-1 samplingRange", {
  exprs <- parse(file.path(testthat::test_path(), "..", "..", "climateYear.R"))
  isFn <- vapply(exprs, function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                   identical(e[[2]], as.name("sampleYear")), logical(1))
  env <- new.env()
  eval(exprs[isFn][[1]], env)
  sampleYear <- get("sampleYear", env)

  Available <- paste0("year", 2001:2005)
  for (i in 1:20) {
    expect_identical(sampleYear(Range = 2003, Starting = NA, Ending = NA, Time = 2001,
                                Available = Available), 2003)
  }
  yrs <- vapply(1:50, function(i) sampleYear(Range = c(2002, 2004), Starting = NA, Ending = NA,
                                             Time = 2001, Available = Available), numeric(1))
  expect_true(all(yrs %in% c(2002, 2004)))
  ## time in range is used as is
  expect_identical(sampleYear(Range = 2001:2005, Starting = NA, Ending = NA, Time = 2004,
                              Available = Available), 2004)
})
