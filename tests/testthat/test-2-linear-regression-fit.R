rm(list = ls(all = TRUE))

library(testthat)
library(jaspTools)
library(mice)
library(dplyr)

setPkgOption("module.dirs", here::here())

setwd(here::here())
source(test_path("setup.R"))

boys <- readRDS(test_path("fixtures", "boys.rds"))
miceMids <- readRDS(test_path("fixtures", "mice_mids.rds"))
options <- readRDS(test_path("fixtures", "lin_reg_options.rds"))

options$modelAICBIC <- TRUE
options$fChange <- TRUE

results <- jaspTools::runAnalysis("MissingDataImputation", boys, options, makeTests = TRUE)

results
options

# Extract and processing the coefficients table from JASP
# coefTab <- results[["results"]][["ModelContainer"]][["collection"]][["ModelContainer_coeffTable"]][["data"]]
# jaspCoefTab <- data.frame(
#   est = sapply(coefTab, "[[", x = "unstandCoeff"),
#   se = sapply(coefTab, "[[", x = "SE"),
#   t = sapply(coefTab, "[[", x = "t"),
#   p = sapply(coefTab, "[[", x = "p")
# )

message("\nTesting external consistency between JASP results and R analyses.\n")

# test_that("ANOVA tables are pooled correctly.", {
#   anovaTab <- results[["results"]][["ModelContainer"]][["collection"]][["ModelContainer_anovaTable"]][["data"]]
#
#   sapply(anovaTab, unlist)
#
#   anovaTab
#
#   mira0 <- with(miceMids, lm(age ~ 1))
#   mira1 <- with(miceMids, lm(age ~ hgt + wgt))
#   mira2 <- with(miceMids, lm(age ~ hgt + wgt + reg))
#
#   fType <- "d1"
#
#   pooledLm0 <- pooledLmObject(mira0, fType = fType)
#   pooledLm1 <- pooledLmObject(mira1, fType = fType)
#   pooledLm2 <- pooledLmObject(mira2, fType = fType)
#
#   mipo0 <- mice::pool(mira0)
#   mipo1 <- mice::pool(mira1)
#   mipo2 <- mice::pool(mira2)
#
#   objList <- list(
#     m0 = pooledLm0,
#     m1 = pooledLm1,
#     m2 = pooledLm2
#   )
#
#   (dfRes <- sapply(objList, df.residual))
#   (msRes <- sapply(objList, \(x) x$pooled$s2))
#   (ranks <- sapply(objList, \(x) x$rank))
#
#   (ssRes <- msRes * dfRes)
#
#   (dfMod <- c(0, diff(ranks)))
#
#   D1(mira1, mira0)
#   D1(mira2, mira1)
#
#   (aov0 <- aov1 <- anova(pooledLm0, pooledLm1, pooledLm2))
#
#   aov1$Res.Df <- dfRes
#   aov1$Df <- dfMod
#   aov1$RSS <- ssRes
#
#   aov1
#
#   fit0 <- mira0$analyses[[1]]
#   fit1 <- mira1$analyses[[1]]
#
#   (aov1 <- anova(fit0, fit1))
#
#   (df0 <- fit0$df.residual)
#   (sigma0 <- summary(fit0)$sigma)
#   (rss0 <- sigma0^2 * df0)
#
#   (df1 <- fit1$df.residual)
#   (sigma1 <- summary(fit1)$sigma)
#   (rss1 <- sigma1^2 * df1)
#
#   aov0
#   anova
#
#   ls(aov0)
#
#   aov0$Df
#
#   (dfR0 <- pooledLm0$df.residual)
#   (dfR1 <- pooledLm1$df.residual)
#   (s2R0 <- pooledLm0$pooled$s2)
#   (s2R1 <- pooledLm1$pooled$s2)
#   (rss0 <- s2R0 * dfR0)
#   (rss1 <- s2R1 * dfR1)
#
#   D1(mira1, mira0)
#
#   options$fType
#
#   anova(mira0, mira1, mira2) |> summary()
#
#   D3(mira1) |> summary()
#   complete(miceMids, "all") |>
#     miceadds::mi.anova("age ~ hgt + wgt")
#
#   anova(mira0, mira1) |> ls()
#
#   pool(mira1) |> summary.aov()
# })

test_that("Stats in the model summary table are pooled correctly.", {})

message("\nTesting internal consistency between JASP Results and JASP State.\n")

# fmt: skip
test_that("ANOVA table results match", {
	table <- results[["results"]][["ModelContainer"]][["collection"]][["ModelContainer_anovaTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(
      "TRUE",  8809.55302668498, 16865.1220122863, 33730.2440245726, "Regression",     2,            "M<unicode>", 0,
      "FALSE", 1.91441290621672, 1373.48699117518, "Residual",       717.445534719818, "M<unicode>",
      "FALSE", 35103.7310157478, "Total",          719.445534719818, "M<unicode>",
      "TRUE",  2935.66244143085, 5613.76308824956, 33682.5785294974, "Regression",     6,            "M<unicode>", 0,
      "FALSE", 1.91226450596731, 1404.0939672624,  "Residual",       734.257192391985,               "M<unicode>",
      "FALSE", 35086.6724967598, "Total",          740.257192391985, "M<unicode>"
			))
})

# fmt: skip
test_that("Model Summary - age table results match", {
	table <- results[["results"]][["ModelContainer"]][["collection"]][["ModelContainer_summaryTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(
      5013.99732819873, 5023.23213415468, "",               0,                 0,                 0,                    6.89405232870225, 0,                 0, 745.008,          "M<unicode>", "",
      2606.46728344939, 2624.93689536129, 8573.05437543385, 0.979712952864344, 0.959837470010173, 0.959837470010173,    1.38362310844273, 0.959729651058869, 2, 717.445534719818, "M<unicode>", 0,
      2609.52286706593, 2646.46209088973, 1.19943474178057, 0.97984600523723,  0.960098193979359, 0.000260723969185972, 1.38284652292556, 0.959775102189887, 4, 734.257192391985, "M<unicode>", 0.309680054509265
    )
  )
})
