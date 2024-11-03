fit_model.log_anova_model <- function(model, trial, c = model$parameters$c) {
  trial <- check_data(trial)

  trial$log_score <- log(trial$Score + c)
  log_anova_model <- lm(trial$log_score ~ trial$Group)

  summary <- summary(log_anova_model)

  ## Extract Coefficients
  sigma_log <- summary$sigma

  mu_control_log <- coef(log_anova_model)[1]
  mu_treatment_log <- sum(coef(log_anova_model))

  estimates <- list(
    mu_control_log = mu_control_log,
    mu_treatment_log = mu_treatment_log,
    sigma_log = sigma_log
  )

  ## Extract treatment effect p_value
  p_value <- summary$coefficients[2, 4]
  std_err <- summary$coefficients[2, 2]

  AIC <- AIC(log_anova_model)

  metrics <- list(
    p_value = p_value,
    std_err = std_err,
    AIC = AIC
  )

  result <- create_fitted_model_result(model, estimates, metrics)

  return(result)
}