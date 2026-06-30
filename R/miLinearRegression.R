#
# Copyright (C) 2026 Utrecht University
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

### ------------------------------------------------------------------------------------------------------------------###

.runRegression <- function(jaspResults, options, ready, lmFunction) {
  impData <- jaspResults[["MiceMids"]]$object |> mice::complete("all")

  # modelContainer <- .linregGetModelContainer(jaspResults, position = 1)
  modelContainer <- jaspResults[["ModelContainer"]]
  model <- jaspRegression:::.linregCalcModel(modelContainer, impData, options, ready, lmFunction)

  ## We need to recompute the F-tests for the R2 changes since they're not pooled correctly in .linregCalcModel()
  model[[1]][["rSquareChange"]] <- .pooledRSquaredChange(fit1 = model[[1]]$fit)

  if (length(options$covariates) + length(options$factors) > 0) {
    for (i in seq_along(model)[-1]) {
      model[[i]][["rSquareChange"]] <- .pooledRSquaredChange(fit1 = model[[i]]$fit, fit0 = model[[i - 1]]$fit)
      # durbinWatson  <- model[[i]][["durbinWatson"]]
    }
  }

  if (is.null(modelContainer[["summaryTable"]])) {
    jaspRegression:::.linregCreateSummaryTable(modelContainer, model, options, position = 1)
  }

  # TODO (KML): Add footnotes about pooling

  if (options$modelFit && is.null(modelContainer[["anovaTable"]])) {
    jaspRegression:::.linregCreateAnovaTable(modelContainer, model, options, position = 2)
  }

  if (options$coefficientEstimate && is.null(modelContainer[["coeffTable"]])) {
    jaspRegression:::.linregCreateCoefficientsTable(modelContainer, model, impData, options, position = 3)
  }

  # TODO (KML): Check what we can do about the bootstrapping, partial cor, and collinearity tables

  # if (options$coefficientBootstrap && is.null(modelContainer[["bootstrapCoeffTable"]]))
  #   jaspRegression:::.linregCreateBootstrapCoefficientsTable(modelContainer, model, dataset, options, position = 4)

  if (options$equationTable && is.null(modelContainer[["equationTable"]])) {
    jaspRegression:::.linregCreateEquationTable(modelContainer, model, dataset, options, position = 4)
  }

  if (options$partAndPartialCorrelation && is.null(modelContainer[["partialCorTable"]])) {
    jaspRegression:::.linregCreatePartialCorrelationsTable(
      modelContainer,
      model,
      impData,
      options,
      position = 5,
      lmFunction = lmFunction
    )
  }

  if (options$covarianceMatrix && is.null(modelContainer[["coeffCovMatrixTable"]])) {
    jaspRegression:::.linregCreateCoefficientsCovarianceMatrixTable(modelContainer, model, options, position = 6)
  }

  # if (options$collinearityDiagnostic && is.null(modelContainer[["collinearityTable"]]))
  #   jaspRegression:::.linregCreateCollinearityDiagnosticsTable(modelContainer, model, options, position = 8)

  if (options$descriptives && is.null(modelContainer[["descriptivesTable"]])) {
    jaspRegression:::.linregCreateDescriptivesTable(modelContainer, impData[[1]], options, position = 5)
    .updateDescriptivesTable(modelContainer[["descriptivesTable"]], impData, options)
  }
}

### ------------------------------------------------------------------------------------------------------------------###

.pooledRSquaredChange <- function(fit1, fit0 = NULL) {
  if (is.null(fit0)) {
    out <- list(
      R2c = NA,
      Fc = NA,
      df1 = 0,
      df2 = fit1$df.residual,
      p = NA
    )
  } else {
    fOut <- fit1$fFun(fit1 = fit1$fits, fit0 = fit0$fits)

    out <- list(
      R2c = fit1$pooled$r2[1, "est"] - fit0$pooled$r2[1, "est"],
      Fc = fOut$result[[1]],
      df1 = fOut$result[[2]],
      df2 = fOut$result[[3]],
      p = fOut$result[[4]]
    )
  }
  out
}

### ------------------------------------------------------------------------------------------------------------------###

.updateDescriptivesTable <- function(descriptivesTable, dataset, options) {
  variables <- c(options$dependent, unlist(options$covariates))
  variables <- variables[variables != ""]

  if (length(variables) > 0) {
    descriptivesTable$setData(NULL)
    descriptivesTable$addRows(.pooledDescriptives(variables, dataset))
  }
}

### ------------------------------------------------------------------------------------------------------------------###

.pooledDescriptives <- function(variables, dataset) {
  descriptives <- vector("list", length(variables))

  for (i in seq_along(variables)) {
    descriptives[[i]] <- list()

    variable <- variables[[i]]
    data <- lapply(dataset, function(x, y) x[[y]], y = variable)

    n <- length(data[[1]])
    v <- sapply(data, var) |> mean()
    b <- sapply(data, mean) |> var()

    descriptives[[i]][["var"]] <- variable
    descriptives[[i]][["N"]] <- n
    descriptives[[i]][["mean"]] <- mean(unlist(data))
    descriptives[[i]][["SD"]] <- sqrt(v)
    descriptives[[i]][["SE"]] <- (sqrt(v) / sqrt(n)) + b + (b / length(data))
  }
  descriptives
}

### ------------------------------------------------------------------------------------------------------------------###

.checkRegressionValidVars <- function(options, jaspResults) {
  regvars <- c(options$dependent, options$covariates, options$factors)
  impvars <- colnames(jaspResults[["MiceMids"]]$object$data)
  if (any(!regvars %in% impvars)) {
    notimputed <- regvars[which(!regvars %in% impvars)]
    stop(
      "The variables ",
      paste0(
        jaspBase::decodeColNames(notimputed),
        collapse = ", ",
        " are not included in the imputation object. If you really don't want to include these variables in the imputation, exclude them through the imputation model specification."
      ),
      call. = FALSE
    )
  }
}
