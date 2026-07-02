# rm(list = ls(all = TRUE))
#
# remotes::install_github("uu-jasp-dev/jaspRegression@missingData")
#
# setupJaspTools()
#
# library(testthat)
# library(mice)
# library(dplyr)
# library(jaspTools)
#
# setPkgOption("module.dirs", here::here())
#
# setwd(here::here())
# source(test_path("setup.R"))

boys <- readRDS(test_path("fixtures", "boys.rds"))
miceMids <- readRDS(test_path("fixtures", "mice_mids.rds"))
options <- readRDS(test_path("fixtures", "lin_reg_options.rds"))

options$descriptives <- TRUE
options$equationTable <- TRUE
options$partAndPartialCorrelation <- TRUE

.numDecimals <<- 3 # jaspRegression seems to be using an undefined variable to format the equations table

results <- jaspTools::runAnalysis("MissingDataImputation", boys, options)

### --------------------------------------------------------------------------------------------------------------------

test_that("JASP descriptives table matches the R version.", {
  jStats <- results[["results"]][["ModelContainer"]][["collection"]][["ModelContainer_descriptivesTable"]][["data"]] |>
    lapply(data.frame) |>
    do.call("rbind", args = _)

  impData <- mice::complete(miceMids, "long") |>
    dplyr::select(tv, hgt, wgt, .imp)

  m0 <- aggregate(impData, by = impData[".imp"], mean) |> dplyr::select(-.imp)
  v0 <- aggregate(impData, by = impData[".imp"], var) |> dplyr::select(-.imp)
  w <- colMeans(v0)
  b <- sapply(m0, var)

  m <- nrow(m0)
  n <- nrow(impData) / m

  data.frame(
    N = n,
    SD = sqrt(w),
    SE = sqrt((w / n) + b + (b / m)),
    mean = colMeans(m0),
    var = names(m0),
    row.names = NULL
  ) |>
    expect_equal(jStats)
})

# fmt: skip
test_that("Descriptives table results match", {
  jaspTools::expect_equal_tables(
    results[["results"]][["ModelContainer"]][["collection"]][["ModelContainer_descriptivesTable"]][["data"]],
    list(
      748, 8.14868775407044, 0.317136961972974, 8.53021390374331, "tv",
      748, 46.5789627197241, 1.70371424364591,  131.075962566845, "hgt",
      748, 26.0375570147206, 0.952274999506044, 37.1557179144385, "wgt"
    )
  )
})

### --------------------------------------------------------------------------------------------------------------------

# fmt: skip
test_that("Regression Equations table results match", {
  jaspTools::expect_equal_tables(
    results[["results"]][["ModelContainer"]][["collection"]][["ModelContainer_equationTable"]][["data"]],
    list(
      "tv = 8.53", "M<unicode>",
      "tv = 0.563 <unicode> 0.0246 hgt + 0.301 wgt", "M<unicode>",
      "tv = 0.239 <unicode> 0.0285 hgt + 0.314 wgt + 0.634 reg (east) <unicode> 2.581 reg (north) + 0.801 reg (south) + 0.899 reg (west)", "M<unicode>"
    )
  )
})

### --------------------------------------------------------------------------------------------------------------------

# fmt: skip
test_that("Part And Partial Correlations table results match", {
  jaspTools::expect_equal_tables(
    results[["results"]][["ModelContainer"]][["collection"]][["ModelContainer_partialCorTable"]][["data"]],
    list(
      "TRUE",  "M<unicode>", "hgt", -0.0487142130288719, -0.0873496894484273,
      "FALSE", "M<unicode>", "wgt", 0.319561941283249,   0.498605744874048,
      "TRUE",  "M<unicode>", "hgt", -0.0558292895593508, -0.102828319178289,
      "FALSE", "M<unicode>", "wgt", 0.330366743956986,   0.521830363365574,
      "FALSE", "M<unicode>", "reg", 0.13032110173177,    0.234576011016636
    )
  )
})
