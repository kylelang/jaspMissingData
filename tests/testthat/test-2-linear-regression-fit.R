rm(list = ls(all = TRUE))

library(testthat)
library(jaspTools)
library(mice)
library(dplyr)

setPkgOption("module.dirs", here::here())

setwd(here::here())
source(test_path("setup.R"))
source(here::here("R/pooledLm.R"))
source(here::here("R/common.R"))

boys <- readRDS(test_path("fixtures", "boys.rds"))
miceMids <- readRDS(test_path("fixtures", "mice_mids.rds"))
options <- readRDS(test_path("fixtures", "lin_reg_options.rds"))

options$modelAICBIC <- TRUE
options$fChange <- TRUE
fFun <- switch(options$fStat, d1 = mice::D1, d2 = mice::D2, d3 = mice::D3)
poolingParms <- with(options, list(fStat = fStat, llEst = llEst))

results <- jaspTools::runAnalysis("MissingDataImputation", boys, options, makeTests = TRUE)

message("\nTesting external consistency between JASP results and R analyses.\n")

jaspAnovaData <- results[["results"]][["ModelContainer"]][["collection"]][["ModelContainer_anovaTable"]][["data"]]

fOut <- rbind.data.frame(
  fFun(fit0 = mira0, fit1 = mira1)$result,
  fFun(fit0 = mira0, fit1 = mira2)$result
)
colnames(fOut) <- c("f", "df1", "df2", "p", "riv")

mira0 <- with(miceMids, lm(tv ~ 1))
mira1 <- with(miceMids, lm(tv ~ hgt + wgt))
mira2 <- with(miceMids, lm(tv ~ hgt + wgt + reg))

pooledLm0 <- pooledLmObject(mira0, pooling = poolingParms)
pooledLm1 <- pooledLmObject(mira1, pooling = poolingParms)
pooledLm2 <- pooledLmObject(mira2, pooling = poolingParms)

mdAov1 <- anova(pooledLm0, pooledLm1)
mdAov2 <- anova(pooledLm0, pooledLm2)

mdF <- c(mdAov1[2, "F"], mdAov2[2, "F"])
jF <- sapply(jaspAnovaData, "[[", x = "F") |> unlist()
test_that("F-Stats in the ANOVA table match the R versions.", {
  expect_equal(jF, fOut$f)
  expect_equal(jF, mdF)
  expect_equal(fOut$f, mdF)
})

tmp <- c(mdAov1[2, "Sum of Sq"], mdAov1$RSS[2])
mdSS <- c(tmp, sum(tmp))
tmp <- c(mdAov2[2, "Sum of Sq"], mdAov2$RSS[2])
mdSS <- c(mdSS, tmp, sum(tmp))

test_that(
  "Sums of squares in the JASP ANOVA table match the R versions.",
  sapply(anovaTab, "[[", x = "SS") |>
    unlist() |>
    expect_equal(mdSS)
)

tmp <- c(mdAov1[2, "Df"], mdAov1$Res.Df[2])
mdDF <- c(tmp, sum(tmp))
tmp <- c(mdAov2[2, "Df"], mdAov2$Res.Df[2])
mdDF <- c(mdDF, tmp, sum(tmp))

tmp <- with(fOut, rbind(df1, df2))
rDF <- rbind(tmp, colSums(tmp)) |> as.numeric()

jDF <- sapply(jaspAnovaData, "[[", x = "df") |> unlist()

test_that("Degrees of freedom in the JASP ANOVA table match the R versions.", {
  expect_equal(jDF, mdDF)
  expect_equal(jDF, rDF)
  expect_equal(rDF, mdDF)
})

test_that(
  "Mean squares in the JASP ANOVA table match the R versions.",
  sapply(anovaTab, "[[", x = "MS") |>
    unlist() |>
    expect_equal({
      mdSS / mdDF
    }[-c(3, 6)])
)

mdP <- c(mdAov1[2, "Pr(>F)"], mdAov2[2, "Pr(>F)"])
jP <- sapply(jaspAnovaData, "[[", x = "p") |> unlist()

test_that("P-values in the JASP ANOVA table match the R versions.", {
  expect_equal(jP, mdP)
  expect_equal(jP, fOut$p)
  expect_equal(fOut$p, mdP)
})

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
