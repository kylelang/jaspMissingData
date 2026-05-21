# rm(list = ls(all = TRUE))

# install.packages(here::here("../../regression/jaspRegression/"), repos = NULL)

# library(testthat)
# library(jaspTools)
# library(mice)
# library(dplyr)

# setPkgOption("module.dirs", here::here())

# setwd(here::here())
# source(test_path("setup.R"))

boys <- readRDS(test_path("fixtures", "boys.rds")) |>
  mutate(across(where(is.ordered), \(x) factor(x, ordered = FALSE)))

midsOut <- mice::mice(boys, maxit = 10, seed = 235711, quiet = TRUE)

# options <- jaspTools::analysisOptions(here::here("../data", "jaspMissingData4.jasp"))
# saveRDS(options, test_path("fixtures", "linRegOptions.rds"))

options <- readRDS(test_path("fixtures", "linRegOptions.rds")) |>
  addImputationVariables(
    variables = colnames(boys),
    methods = mice::make.method(boys),
    types = c("scale", "scale", "scale", "scale", "scale", "ordinal", "ordinal", "scale", "nominal")
  )

results <- jaspTools::runAnalysis("MissingDataImputation", boys, options, makeTests = TRUE)

message("Testing external consistency between JASP results and R analyses.")

test_that("Linear regression parameters are pooled correctly.", {
  # Extract and processing the coefficients table from JASP
  coefTab <- results[["results"]][["ModelContainer"]][["collection"]][["ModelContainer_coeffTable"]][["data"]]
  jaspCoefTab <- data.frame(
    est = sapply(coefTab, "[[", x = "unstandCoeff"),
    se = sapply(coefTab, "[[", x = "SE"),
    t = sapply(coefTab, "[[", x = "t"),
    p = sapply(coefTab, "[[", x = "p")
  )

  # Intercept only model
  mipoCoef <- with(midsOut, lm(age ~ 1)) |>
    pool() |>
    summary() |>
    select(-term, -df) |>
    as.data.frame()

  colnames(mipoCoef) <- c("est", "se", "t", "p")

  expect_equal(head(jaspCoefTab, 1), mipoCoef)

  # Only numeric predictors
  mipoCoef <- with(midsOut, lm(age ~ hgt + wgt)) |>
    pool() |>
    summary() |>
    select(-term, -df) |>
    as.data.frame()

  colnames(mipoCoef) <- c("est", "se", "t", "p")

  jaspCoefTab[2:4, ] |>
    data.frame(row.names = 1:3) |>
    expect_equal(mipoCoef)

  # Numeric and categorical predictors
  mipoCoef <- with(midsOut, lm(age ~ hgt + wgt + gen)) |>
    pool() |>
    summary() |>
    select(-term, -df) |>
    as.data.frame()

  colnames(mipoCoef) <- c("est", "se", "t", "p")

  tail(jaspCoefTab, 7) |>
    data.frame(row.names = 1:7) |>
    expect_equal(mipoCoef)
})

test_that("ANOVA tables are pooled correctly.", {
  # anovaTab <- results[["results"]][["ModelContainer"]][["collection"]][["ModelContainer_anovaTable"]][["data"]]
  #
  # sapply(anovaTab, unlist)
  #
  # anovaTab
  #
  # mira0 <- with(midsOut, lm(age ~ 1))
  # mira1 <- with(midsOut, lm(age ~ hgt + wgt))
  # mira2 <- with(midsOut, lm(age ~ hgt + wgt + gen))
  #
  # anova(mira0, mira1, mira2) |> summary()
  #
  # D3(mira1) |> summary()
  # complete(midsOut, "all") |>
  #   miceadds::mi.anova("age ~ hgt + wgt")
  #
  # anova(mira0, mira1) |> ls()
  #
  # pool(mira1) |> summary.aov()
})

test_that("Stats in the model summary table are pooled correctly.", {})
test_that("The coefficients covariance matrices are pooled correctly.", {})

message("Testing internal consistency between JASP Results and JASP State.")

results <- runAnalysis("MissingDataImputation", boys, options)

# fmt: skip
test_that("ANOVA table results match", {
	table <- results[["results"]][["ModelContainer"]][["collection"]][["ModelContainer_anovaTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(
      "TRUE", 1184.86526007729, 2260.59183766517, 4521.18367533035, "Regression", 2, "M<unicode>", 0,
      "FALSE", 1.90788937260066, 18027.2852032929, "Residual", 9448.81053492103, "M<unicode>",
      "FALSE", 22548.4688786233, "Total", 9450.81053492103, "M<unicode>",
      "TRUE", 328.055572496645, 515.315393923291, 3091.89236353975, "Regression", 6, "M<unicode>", 4.08872332579331e-137,
      "FALSE", 1.57081737707278, 527.297760578658, "Residual", 335.683681804741, "M<unicode>",
      "FALSE", 3619.19012411841, "Total", 341.683681804741, "M<unicode>")
  )
})

# fmt: skip
test_that("Coefficients Covariance Matrix table results match", {
	table <- results[["results"]][["ModelContainer"]][["collection"]][["ModelContainer_coeffCovMatrixTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(
      "TRUE", "", "", "", "", 1.09420757317606e-05, "M<unicode>", "hgt", -1.84939019065746e-05,
      "FALSE", "", "", "", "", "", "M<unicode>", "wgt", 3.50714031279474e-05,
      "TRUE", -0.000251446809534406, -9.32286902459143e-05, -0.000127218762367469, 2.41806801776141e-06, 1.0854915176445e-05, "M<unicode>", "hgt", -1.74078220205472e-05,
      "FALSE", 2.86268210744396e-05, -0.000428782053617189, -0.000413537150221638, -0.000846606582059153, "", "M<unicode>", "wgt", 4.28843202913443e-05,
      "FALSE", 0.0466427206042838, 0.0177290275742955, 0.0239688642528287, 0.0240606409837029, "", "M<unicode>", "gen (G2)", "",
      "FALSE", "", 0.0806701056003807, 0.0294026015602966, 0.0373319834043673, "", "M<unicode>", "gen (G3)", "",
      "FALSE", "", "", 0.0596614477252614, 0.0315515907964428, "", "M<unicode>", "gen (G4)", "",
      "FALSE", "", "", "", 0.0677013621415581, "", "M<unicode>", "gen (G5)", "")
  )
})

# fmt: skip
test_that("Coefficients table results match", {
	table <- results[["results"]][["ModelContainer"]][["collection"]][["ModelContainer_coeffTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(
      "FALSE", 0.252071517923469, "M<unicode>", "(Intercept)", 4.14597392666055e-167, 36.3343958318255, 9.15886631016043,
      "TRUE", 0.242583415488217, "M<unicode>", "(Intercept)", 3.94890181582257e-131, -30.6786580521206, -7.44213365287851,
      "FALSE", 0.0033078808521107, "M<unicode>", "hgt", 1.65459231349572e-138, "", 32.0873109148236, 0.106141001370868,
      "FALSE", 0.00592211137415934, "M<unicode>", "wgt", 2.9475997817773e-31, "", 12.2345924965817, 0.0724546193822112,
      "TRUE", 0.239562193463648, "M<unicode>", "(Intercept)", 3.79316545336672e-109, -28.8658950563774, -6.91517713599724,
      "FALSE", 0.00329467982912529, "M<unicode>", "hgt", 1.81751051228229e-111, "", 31.8891884297064, 0.105064665886529,
      "FALSE", 0.0065486120889349, "M<unicode>", "wgt", 2.59542041791162e-07, "", 5.24884155046202, 0.0343726272302594,
      "FALSE", 0.215969258470468, "M<unicode>", "gen (G2)", 2.05549858146361e-08, 5.99488434350368, 1.29471072628271,
      "FALSE", 0.284024832717811, "M<unicode>", "gen (G3)", 2.96928156925409e-06, 4.87084153122639, 1.38343995110154,
      "FALSE", 0.244256929738465, "M<unicode>", "gen (G4)", 1.39812828140732e-13, 7.67221647950416, 1.87399204157254,
      "FALSE", 0.260194854179628, "M<unicode>", "gen (G5)", 9.9703156474258e-18, 11.1290497719699, 2.89572148257554)
  )
})

# fmt: skip
test_that("Model Summary - age table results match", {
	table <- results[["results"]][["ModelContainer"]][["collection"]][["ModelContainer_summaryTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(
      5013.99732819873, 5023.23213415468, "", 0, 0, 0, 6.89405232870225, 0, 0, 745.008, "M<unicode>", "",
      2604.93431462884, 2623.40392654074, 113287.336127756, 0.979778878460468, 0.959966650677252, 0.959966650677252, 1.38126368684645, 0.959859178584724, 2, 9448.81053492103, "M<unicode>", 0,
      2431.36583128479, 2468.30505510858, 18.5938832247468, 0.983477402496113, 0.967227801220502, 0.00726115054325016, 1.25332253513323, 0.966962438977968, 4, 335.683681804741, "M<unicode>", 8.11560538734583e-14)
  )
})
