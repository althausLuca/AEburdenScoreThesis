source("R/models/models.R")
source("R/models/fit_models.R")
source("R/models/model_CDFs.R")
source("R/trials/trial_loader.R")


model <- TWEEDIE_REGRESSION(var_power = "infer", link_power = 1)
trial_data <- load_shorter_trials()


x <- c(seq(-10,20, by = 0.1), seq(20, 100, by = 0.5), seq(100, 200, by = 1), seq(200, 600, by = 10))

treatment_results <- vector("list", length(trial_data$n_trials))
control_results <- vector("list", length(trial_data$n_trials))

coef_df <- NULL

for (i in seq_along(trial_data$trials)) {
  trial <- trial_data$trials[[i]]
  fitted_model <- fit_model(model, trial)
  model_coefs <- extract_coefficients(fitted_model)
  coef(fitted_model, what = "sigma")
  if (is.null(coef_df)) {
    coef_df <- as.data.frame(t(unlist(model_coefs)))
  } else {
    coef_df <- rbind(coef_df, model_coefs)
  }

  distributions <- model_distribution(fitted_model, x)
  treatment_dist <- distributions$treatment
  control_dist <- distributions$control

  treatment_results[[i]] <- treatment_dist
  control_results[[i]] <- control_dist
}


treatment_result_matrix <- do.call(rbind, treatment_results)
control_result_matrix <- do.call(rbind, control_results)


distribution_results <- list(
  treatment_results = treatment_result_matrix,
  control_results = control_result_matrix,
  x = x,
  model = model,
  trial_data = trial_data,
  coef_df = coef_df
)

save(distribution_results, file = paste0("data/distributions/longer/", model$repr, ".RData"))

