source(test_path("helper.R"))

inPath <- here::here("..", "data", "jaspMissingData1.jasp")
outFile <- "lin_reg_options.rds"

jaspTools::analysisOptions(inPath) |>
  addImputationVariables(
    variables = colnames(boys),
    methods = mice::make.method(boys),
    types = c("scale", "scale", "scale", "scale", "scale", "ordinal", "ordinal", "scale", "nominal")
  ) |>
  saveRDS(test_path("fixtures", outFile))
