
source("R/trials/trial_loader.R")
source("R/models_and_tests/models_and_tests.R")
x <- source("R/evaluation/distribution_fit/x.R")$value

trial_data <- load_longer_trials()

model <- TWEEDIE_REGRESSION(xi = "infer")

trial <- trial_data$trials[[5]]

control_distributions <- vector("list", length(trial_data$trials))
treatment_distributions <- vector("list", length(trial_data$trials))

for(trial_index in seq_along(trial_data$trials)){
  trial <- trial_data$trials[[trial_index]]
  model_fit <- model$fit(trial)
  model_CDFs <- model_fit$get_CDFs(x)
  treatment_CDF <- model_CDFs$treatment
  control_CDF <- model_CDFs$control
    treatment_distributions[[trial_index]] <- treatment_CDF
    control_distributions[[trial_index]] <- control_CDF
}



par(mfrow=c(2,1))
plot(x,treatment_distribution)
# plot emprical distribution of treatment_y
lines(x, ecdf(treatment_y)(x))

plot(x,control_distribution)
# plot emprical distribution of control_y
lines(x, ecdf(control_y)(x))