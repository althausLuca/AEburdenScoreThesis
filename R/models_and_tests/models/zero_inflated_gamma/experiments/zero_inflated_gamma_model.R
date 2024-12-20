library(gamlss)

source("../../../trials/trial_loader.R")
source("../../fit_models.R")
source("../../model_coefficients.R")
source("../../model_metrics.R")


trial_data <- load_shorter_trials()
trial <- trial_data$trials[[1]]

model_fit <- fit_zero_inflated_gamma_model(trial)


summary(model_fit)
AIC(model_fit)
coefficients <- extract_coefficients(model_fit)
p_values <- extract_gamlss_p_values(model_fit)


