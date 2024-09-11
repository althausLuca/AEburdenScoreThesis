source("R/models_and_tests/model_helpers.R")

#init model fit functions
fit_model <- function(model, trial, ...) {
  UseMethod("fit_model")
}

fit_model.default <- function(model, trial, ...) {
  stop(paste0("No fit function for model", model$name, " found, model has class", class(model)))
}

run_test <- function(test, trial, ...) {
  UseMethod("run_test")
}

run_test.default <- function(test, trial, ...) {
  stop(paste0("No run_test function for test ", test$name, " found, test has class ", class(test)))
}


#source fit functions for models
sapply(grep("fit_model.R", list.files("R/models_and_tests", full.names = TRUE, recursive = TRUE), value = TRUE)
  , source)

#source run functions for tests
sapply(grep("run_test.R", list.files("R/models_and_tests", full.names = TRUE, recursive = TRUE), value = TRUE)
  , source)


#' @title set the force computation flag for a model or test
#' @param model model or test object to force the model_computer to recompute the results
#' @return model or test object with force_computation flag set to TRUE
force_computation <- function(model) {
  model$force_computation <- TRUE
  return(model)
}

