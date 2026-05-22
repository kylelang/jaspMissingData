## Make sure all functions defined in R/* are instantiated and available for testing
devtools::load_all()

message("Generating a fresh mice MIDS object to supply reference values.")

readRDS(test_path("fixtures", "boys.rds")) |>
  mice::mice(maxit = 10, seed = 235711, printFlag = FALSE) |>
  saveRDS(test_path("fixtures", "mice_mids.rds"))
