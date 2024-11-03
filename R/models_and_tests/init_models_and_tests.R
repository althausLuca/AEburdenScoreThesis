source("R/models_and_tests/model_helpers.R")
source("R/models_and_tests/models/LRT_test.R")

#init model fit functions
fit_model <- function(model, trial, ...) {
  UseMethod("fit_model")
}

fit_model.default <- function(model, trial, ...) {
  stop(paste0("No fit function for model", model$name, " found, model has class", class(model)))
}

#init run_test functions
run_test <- function(test, trial, ...) {
  UseMethod("run_test")
}

run_test.default <- function(test, trial, ...) {
  stop(paste0("No run_test function for test ", test$name, " found, test has class ", class(test)))
}

#init get_CDFs functions
get_CDFs <- function(fitted_model, x, estimates , ...) {
  UseMethod("get_CDFs")
}

get_CDFs.default <- function(fitted_model, x, estimates , ...) {
  stop(paste0("No get_CDFs function for model ", class(fitted_model), " found, model has class ", class(fitted_model)))
}

#source fit_model functions for models
sapply(grep("fit_model.R", list.files("R/models_and_tests", full.names = TRUE, recursive = TRUE), value = TRUE)
  , source)

#source run_test functions for tests
sapply(grep("run_test.R", list.files("R/models_and_tests", full.names = TRUE, recursive = TRUE), value = TRUE)
  , source)

# source get_CDFs functions for models
sapply(grep("get_CDFs.R", list.files("R/models_and_tests", full.names = TRUE, recursive = TRUE), value = TRUE)
  , source)



