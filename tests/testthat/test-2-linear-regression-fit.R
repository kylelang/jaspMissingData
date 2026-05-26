# rm(list = ls(all = TRUE))
#
# remotes::install_github("jasp-stats/jaspTools")
#
# library(testthat)
# library(jaspTools)
# library(mice)
# library(dplyr)
#
# setupJaspTools(
#   pathJaspDesktop = "~/data/software/jasp/desktop/jasp-desktop",
#   installJaspModules = FALSE,
#   installJaspCorePkgs = TRUE
# )
#
# setPkgOption("module.dirs", here::here())
#
# setwd(here::here())
# source(test_path("setup.R"))
# source(here::here("R/pooledLm.R"))
# source(here::here("R/common.R"))

boys <- readRDS(test_path("fixtures", "boys.rds"))
miceMids <- readRDS(test_path("fixtures", "mice_mids.rds"))
options <- readRDS(test_path("fixtures", "lin_reg_options.rds"))

options$modelAICBIC <- TRUE
options$fChange <- TRUE

### --------------------------------------------------------------------------------------------------------------------

## Some previous test nukes the environment, so we need to re-export all our private functions
devtools::load_all()

mira0 <- with(miceMids, lm(tv ~ 1))
mira1 <- with(miceMids, lm(tv ~ hgt + wgt))
mira2 <- with(miceMids, lm(tv ~ hgt + wgt + reg))

fFun <- switch(options$fStat, d1 = mice::D1, d2 = mice::D2, d3 = mice::D3)
fOut <- rbind.data.frame(
  fFun(fit0 = mira0, fit1 = mira1)$result,
  fFun(fit0 = mira0, fit1 = mira2)$result
)
colnames(fOut) <- c("f", "df1", "df2", "p", "riv")

poolingParms <- with(options, list(fStat = fStat, llEst = llEst))
pooledLm0 <- pooledLmObject(mira0, pooling = poolingParms)
pooledLm1 <- pooledLmObject(mira1, pooling = poolingParms)
pooledLm2 <- pooledLmObject(mira2, pooling = poolingParms)

mdAov1 <- anova.pooledlm(pooledLm0, pooledLm1)
mdAov2 <- anova.pooledlm(pooledLm0, pooledLm2)

### --------------------------------------------------------------------------------------------------------------------

results <- jaspTools::runAnalysis("MissingDataImputation", boys, options)
jaspAnovaData <- results[["results"]][["ModelContainer"]][["collection"]][["ModelContainer_anovaTable"]][["data"]]

### --------------------------------------------------------------------------------------------------------------------

# message("\nTesting external consistency between JASP results and R analyses.\n")

### --------------------------------------------------------------------------------------------------------------------

mdF <- c(mdAov1[2, "F"], mdAov2[2, "F"])
jF <- sapply(jaspAnovaData, "[[", x = "F") |> unlist()

test_that("F-Stats in the ANOVA table match the R versions.", {
  expect_equal(jF, fOut$f)
  expect_equal(jF, mdF)
  expect_equal(fOut$f, mdF)
})

### --------------------------------------------------------------------------------------------------------------------

tmp <- c(mdAov1[2, "Sum of Sq"], mdAov1$RSS[2])
mdSS <- c(tmp, sum(tmp))
tmp <- c(mdAov2[2, "Sum of Sq"], mdAov2$RSS[2])
mdSS <- c(mdSS, tmp, sum(tmp))

test_that(
  "Sums of squares in the JASP ANOVA table match the R versions.",
  sapply(jaspAnovaData, "[[", x = "SS") |>
    unlist() |>
    expect_equal(mdSS)
)

### --------------------------------------------------------------------------------------------------------------------

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

### --------------------------------------------------------------------------------------------------------------------

test_that(
  "Mean squares in the JASP ANOVA table match the R versions.",
  sapply(jaspAnovaData, "[[", x = "MS") |>
    unlist() |>
    expect_equal({
      mdSS / mdDF
    }[-c(3, 6)])
)

### --------------------------------------------------------------------------------------------------------------------

mdP <- c(mdAov1[2, "Pr(>F)"], mdAov2[2, "Pr(>F)"])
jP <- sapply(jaspAnovaData, "[[", x = "p") |> unlist()

test_that("P-values in the JASP ANOVA table match the R versions.", {
  expect_equal(jP, mdP)
  expect_equal(jP, fOut$p)
  expect_equal(fOut$p, mdP)
})

### --------------------------------------------------------------------------------------------------------------------

test_that("Stats in the model summary table are pooled correctly.", {})

### --------------------------------------------------------------------------------------------------------------------

# message("\nTesting internal consistency between JASP Results and JASP State.\n")

### --------------------------------------------------------------------------------------------------------------------

# fmt: skip
test_that("ANOVA table results match", {
	table <- results[["results"]][["ModelContainer"]][["collection"]][["ModelContainer_anovaTable"]][["data"]]
	jaspTools::expect_equal_tables(
    table,
		list(
      "TRUE",  288.726739159353, 6145.34464763188, 12290.6892952638, "Regression", 2,                "M<unicode>", 2.62000824959559e-19,
      "FALSE", 21.2842934655947, 591.011756652411,                   "Residual",   27.7675064764428, "M<unicode>",
      "FALSE", 12881.7010519162,                                     "Total",      29.7675064764428, "M<unicode>",
      "TRUE",  96.6427403180656, 1954.36468718679, 11726.1881231207, "Regression", 6,                "M<unicode>", 1.66585523137147e-46,
      "FALSE", 20.2225710979912, 2742.81723307344,                   "Residual",   135.631479290281, "M<unicode>",
      "FALSE", 14469.0053561942,                                     "Total",      141.631479290281, "M<unicode>"
    )
  )
})

### --------------------------------------------------------------------------------------------------------------------

# fmt: skip
test_that("Model Summary - tv table results match", {
  table <- results[["results"]][["ModelContainer"]][["collection"]][["ModelContainer_summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      5209.65301610799, 5218.88782206394, "",               0,                 0,                 "",                 8.20888313504352, 0,                 0, 201.429231532675, "M<unicode>", "",
      4195.25372919822, 4213.72334111012, 288.726739159353, 0.831476298586239, 0.691352835110672, 0.691352835110672,  4.61349037775031, 0.690524102452899, 2, 27.7675064764428, "M<unicode>", 2.62000824959559e-19,
      4149.7041616798,  4186.6433855036,  6.28797056778446, 0.841627248054181, 0.708336424667255, 0.0169835895565824, 4.49695131149885, 0.705974382467772, 4, 74.1048688479238, "M<unicode>", 0.000206126409128476
    )
  )
})
