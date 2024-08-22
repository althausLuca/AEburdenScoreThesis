
library(gamlss)
source("R/models/model_helpers.R")
source("R/models/zero_inflated_gamma/LRT_test.R")

fit_zero_inflated_gamma_model <- function(trial , sigma_per_group = FALSE) {
  trial <- check_data(trial)

  gamlss_model <- tryCatch({
    if (sigma_per_group) {
      gamlss(Score ~ Group, sigma.formula = ~Group, nu.formula = ~Group,
             family = ZAGA, data = trial)
    } else {
      gamlss(Score ~ Group, sigma.formula = ~1, nu.formula = ~Group,
             family = ZAGA, data = trial)
    }
  }, error = function(e) {
    # Return an error classed object if the model fitting fails
    structure(list(message = e$message), class = c("gamlss_error", "model_error"))
  })

  # extract coefficients
  mu_coefficients <- coef(gamlss_model, what = "mu")
  nu_coefficients <- coef(gamlss_model, what = "nu")
  sigma_coefficients <- coef(gamlss_model, what = "sigma")

  logit_inv <- function(x) { 1 / (1 + exp(-x)) }


  estimates <- list(
    mu_control = exp(mu_coefficients[1]),
    mu_treatment = exp(sum(mu_coefficients)),
    nu_control = unname(logit_inv(nu_coefficients[1])),
    nu_treatment = logit_inv(sum(nu_coefficients))
  )

  if (sigma_per_group) {
    estimates$sigma_control <- unname(exp(sigma_coefficients[1]))
    estimates$sigma_treatment <- exp(sum(sigma_coefficients))
  }
  else {
    estimates$sigma <- unname(exp(sigma_coefficients[1]))
  }

  model_summary <- summary(gamlss_model)
  mu_p_val <- model_summary[2, 4]
  nu_p_val <- model_summary[5+sigma_per_group, 4]
  AIC <- AIC(gamlss_model)

  print("Running LRT test zero-inflated gamma model")
  p_value <- LRT_test(trial, dist = "gamma", fix_arg = TRUE)

  result <- list(
    model = gamlss_model,
    estimates = estimates,
    mu_p_value = mu_p_val,
    nu_p_value = nu_p_val,
    p_value = p_value,
    AIC = AIC
  )
}
