#' This file is used to load the trial data, one  trial and the models and tests for the user to work with
#' mostly to quickly try out things
source("R/trials/trial_loader.R")
if(!exists("fit_model")){
  source("R/models_and_tests/models_and_tests.R")
}

print("loading trial data")

trial_data <- load_shorter_trials()
trial <- trial_data$trials[[1]]

