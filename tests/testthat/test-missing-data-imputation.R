context("Missing Data Imputation")

boysdata <- readRDS("boys.RDS")

options <- jaspTools::analysisOptions("MissingDataImputation")
options$tracePlot <- TRUE

options$imputationVariables <- list(
  optionKey = "variable",
  types = c("scale", "scale", "scale", "scale", "scale", "ordinal", "ordinal", "scale", "nominal"),
  value = list(
    list(method = "pmm", variable = "age"),
    list(method = "pmm", variable = "hgt"),
    list(method = "pmm", variable = "wgt"),
    list(method = "pmm", variable = "bmi"),
    list(method = "pmm", variable = "hc"),
    list(method = "polr", variable = "gen"),
    list(method = "polr", variable = "phb"),
    list(method = "pmm", variable = "tv"),
    list(method = "polyreg", variable = "reg")
  )
)

result <- jaspTools::runAnalysis("MissingDataImputation", boysdata, options)

imp <- mice::mice(
  boysdata,
  m = options$nImp,
  maxit = options$nIter,
  method = sapply(options$imputationVariables$value, \(x) x$method),
  formulas = mice::make.formulas(boysdata),
  seed = options$seed,
  print = FALSE
)

testplot <- result$state$figures[[1]]$obj
testtable <- result$state$other[[1]]$imp$hgt

jaspTools::expect_equal_plots(
  testplot,
  "convergence"
)

jaspTools::expect_equal_tables(
  testtable,
  list(60.6, 91.1, 78, 78.7, 86.5, 78, 78.5, 69.8, 78.5, 76.5, 81.1,
       76.5, 84, 91.6, 80, 81, 84.5, 111.1, 148.1, 186, 60, 80, 74.2,
       91.2, 80, 80, 64, 87.5, 85, 86.5, 94.2, 64, 84.5, 82.5, 80,
       91.1, 75.2, 132, 161.4, 173.5, 58.5, 81.2, 83, 68, 77, 81.5,
       82.9, 85.5, 93.1, 80, 93.1, 81, 95.5, 80, 95.5, 79, 84.5, 108,
       156.7, 181.8, 50.1, 78, 94, 83.7, 81, 88.4, 80, 78.6, 92.5,
       88.3, 84, 80, 91.2, 77, 77.2, 91.2, 85.5, 95.3, 155, 182.1,
       57.5, 64, 81.2, 81, 87, 91.6, 77.5, 77, 91, 77, 94.2, 88.4,
       84, 85.3, 93.1, 85.5, 85.5, 138, 159.4, 187.6)
)
