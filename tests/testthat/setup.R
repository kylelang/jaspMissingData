## Make sure all functions defined in R/* are instantiated and available for testing
devtools::load_all()

midsPath <- testthat::test_path("fixtures", "mice_mids.rds")

## Check if we need to refresh the MIDS fixture
if (file.exists(midsPath)) {
  midsFixture <- readRDS(midsPath)
  refreshMids <- midsFixture$version != packageVersion("mice")
} else {
  refreshMids <- TRUE
}

if (refreshMids) {
  message("Generating a fresh mice MIDS object to supply reference values.")

  readRDS(testthat::test_path("fixtures", "boys.rds")) |>
    mice::mice(maxit = 10, seed = 235711, printFlag = FALSE) |>
    saveRDS(midsPath)
}
