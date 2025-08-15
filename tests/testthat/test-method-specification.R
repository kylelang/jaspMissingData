
test_that(".makeMethodVector works", {
  data <- mice::boys[,c("hgt", "wgt", "gen")]
  options <- list(
    imputationTargets = c("hgt", "wgt", "gen"),
    imputationMethods = c("pmm", "pmm", "logistic")
  )
  expect_equal(
    .makeMethodVector(data, options),
    c("pmm", "pmm", "polyreg") |> setNames(colnames(data))
  )
  options$imputationMethods <- c("pmm", "norm", "pmm")
  expect_equal(
    .makeMethodVector(data, options),
    c("pmm", "norm", "pmm") |> setNames(colnames(data))
  )
  options$imputationMethods <- c("pmm", "logistic", "logistic")
  expect_equal(
    .makeMethodVector(data, options),
    c("pmm", "polyreg", "polyreg") |> setNames(colnames(data))
  )
})
