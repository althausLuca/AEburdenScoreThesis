#' This file is used to load some trial data, one  trial and the models and tests
#' to quickly try out/tests things
source("R/evaluation/config.R")
source("R/models_and_tests/models_and_tests.R")
source("R/models_and_tests/model_computer.R")

model_computer <- load_model_computer(DEFAULT_DURATION_VAR_FILE)
names(model_computer$models)

model <- ZERO_INFLATED_NORMAL()
add_model(model_computer, model,save=FALSE)
model_computer$model_metrics[[model$repr]]

model.2 <- ZERO_INFLATED_NORMAL(sigma_per_group = TRUE)
add_model(model_computer, model.2)

unlist(model_computer$model_metrics[[model.2$repr]]$AIC) - unlist(model_computer$model_metrics[[model$repr]]$AIC)
