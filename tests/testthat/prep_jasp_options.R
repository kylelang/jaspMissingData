# here::here("tests", "testthat", "helper.R") |> source()
#
# inPath <- here::here("..", "data", "jaspMissingData00.jasp")
# outFile <- "mi_options.rds"
#
# boys <- readRDS(here::here("tests", "testthat", "fixtures", "boys.rds"))
#
# jaspTools::analysisOptions(inPath) |>
#   addImputationVariables(
#     variables = colnames(boys),
#     methods = mice::make.method(boys),
#     types = c("scale", "scale", "scale", "scale", "scale", "ordinal", "ordinal", "scale", "nominal")
#   ) |>
#   saveRDS(here::here("tests", "testthat", "fixtures", outFile))
