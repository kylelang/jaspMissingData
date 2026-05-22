# rm(list = ls(all = TRUE))
#
# library(testthat)
# library(jaspTools)
# library(mice)
# library(dplyr)
#
# setPkgOption("module.dirs", here::here())
#
# setwd(here::here())
# source(test_path("setup.R"))

boys <- readRDS(test_path("fixtures", "boys.rds"))
miceMids <- readRDS(test_path("fixtures", "mice_mids.rds"))
options <- readRDS(test_path("fixtures", "lin_reg_options.rds"))

options$covarianceMatrix <- TRUE

results <- jaspTools::runAnalysis("MissingDataImputation", boys, options)

# Extract and processing the coefficients table from JASP
coefTab <- results[["results"]][["ModelContainer"]][["collection"]][["ModelContainer_coeffTable"]][["data"]]
jaspCoefTab <- data.frame(
  est = sapply(coefTab, "[[", x = "unstandCoeff"),
  se = sapply(coefTab, "[[", x = "SE"),
  t = sapply(coefTab, "[[", x = "t"),
  p = sapply(coefTab, "[[", x = "p")
)

message("\nTesting external consistency between JASP results and R analyses.\n")

test_that("Linear regression parameters pooled correctly for an intercept-only model.", {
  mipoCoef <- with(miceMids, lm(age ~ 1)) |>
    mice::pool() |>
    summary() |>
    dplyr::select(-term, -df) |>
    as.data.frame()

  colnames(mipoCoef) <- c("est", "se", "t", "p")

  expect_equal(head(jaspCoefTab, 1), mipoCoef)
})

test_that("Linear regression parameters pool correctly with only numeric predictors.", {
  mipoCoef <- with(miceMids, lm(age ~ hgt + wgt)) |>
    mice::pool() |>
    summary() |>
    dplyr::select(-term, -df) |>
    as.data.frame()

  colnames(mipoCoef) <- c("est", "se", "t", "p")

  jaspCoefTab[2:4, ] |>
    data.frame(row.names = 1:3) |>
    expect_equal(mipoCoef)
})

test_that("Linear regression parameters pool correctly with numeric and categorical predictors.", {
  mipoCoef <- with(miceMids, lm(age ~ hgt + wgt + reg)) |>
    mice::pool() |>
    summary() |>
    dplyr::select(-term, -df) |>
    as.data.frame()

  colnames(mipoCoef) <- c("est", "se", "t", "p")

  tail(jaspCoefTab, 7) |>
    data.frame(row.names = 1:7) |>
    expect_equal(mipoCoef)
})

test_that("The coefficients covariance matrices are pooled correctly.", {})

message("\nTesting internal consistency between JASP Results and JASP State.\n")

# fmt: skip
test_that("Coefficients table results match", {
	table <- results[["results"]][["ModelContainer"]][["collection"]][["ModelContainer_coeffTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(
      "FALSE", 0.252071517923469,   "M<unicode>", "(Intercept)", 4.14597392666055e-167, 36.3343958318255,   9.15886631016043,
      "TRUE",  0.241388759426161,   "M<unicode>", "(Intercept)", 1.68937381158325e-134, -30.8245454235753,  -7.4406987796722,
      "FALSE", 0.00329285540403064, "M<unicode>", "hgt",         1.3510793810701e-141,  "",                 32.1983800724375,   0.106024609822558,
      "FALSE", 0.0059037839054454,  "M<unicode>", "wgt",         1.0311700726364e-31,   "",                 12.3190515534348,   0.0727290182915203,
      "TRUE",  0.283844994940581,   "M<unicode>", "(Intercept)", 3.4656015579067e-105,  -26.1715382174594,  -7.42866013292199,
      "FALSE", 0.00331246049278279, "M<unicode>", "hgt",         6.30628523659042e-140, "",                 32.0630791748417,   0.10620768304363,
      "FALSE", 0.00596332953292449, "M<unicode>", "wgt",         3.03911320002771e-31,  "",                 12.2307919383193,   0.0729362427768346,
      "FALSE", 0.196581101837589,   "M<unicode>", "reg (east)",  0.347699326358049,     -0.939683101038365, -0.184723939380284,
      "FALSE", 0.226961293336746,   "M<unicode>", "reg (north)", 0.294377304534344,     -1.04937734135525,  -0.238168038592265,
      "FALSE", 0.192056159502992,   "M<unicode>", "reg (south)", 0.85882911542211,      0.177933459213752,  0.0341732168236755,
      "FALSE", 0.186192907561261,   "M<unicode>", "reg (west)",  0.824114378718915,     0.222340218659345,  0.04139817177999
    )
  )
})

# fmt: skip
test_that("Coefficients Covariance Matrix table results match", {
	table <- results[["results"]][["ModelContainer"]][["collection"]][["ModelContainer_coeffCovMatrixTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(
      "TRUE",  1.08428967118538e-05, "M<unicode>", "hgt",         "",                   "",                    "",                    "",                    -1.83447079756992e-05,
      "FALSE", "",                   "M<unicode>", "wgt",         "",                   "",                    "",                    "",                    3.48546644021962e-05,
      "TRUE",  1.09723945162468e-05, "M<unicode>", "hgt",         -4.0104533378228e-05, 2.20079655817134e-05,  -8.22367429248015e-06, -2.76416012468491e-06, -1.86292118150652e-05,
      "FALSE", "",                   "M<unicode>", "wgt",         5.74441347640765e-05, -0.000100127363250247, 9.71982170588439e-06,  -4.49917245205374e-06, 3.55612991182494e-05,
      "FALSE", "",                   "M<unicode>", "reg (east)",  0.0386441295996804,   0.0268107429684344,    0.0266264533088814,    0.0266363479001463,    "",
      "FALSE", "",                   "M<unicode>", "reg (north)", "",                   0.0515114286730887,    0.0269914912993604,    0.0268883951427532,    "",
      "FALSE", "",                   "M<unicode>", "reg (south)", "",                   "",                    0.0368855684030386,    0.0266489303494129,    "",
      "FALSE", "",                   "M<unicode>", "reg (west)",  "",                   "",                    "",                    0.0346677988261163,    ""
    )
  )
})
