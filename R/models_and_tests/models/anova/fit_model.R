fit_model.anova_model <- function(model, trial) {
  trial <- check_data(trial)

  ##model fitting
  anova_model <- lm(trial$Score ~ trial$Group)
  summary <- summary(anova_model)

  ## Extract Coefficients
  sigma <- summary$sigma
  mu_control <- coef(anova_model)[1]
  mu_treatment <- sum(coef(anova_model))

  estimates <- list(
    mu_control = mu_control,
    mu_treatment = mu_treatment,
    sigma = sigma
  )

  ## Extract treatment effect p_value
  p_value <- summary$coefficients[2, 4]
  std_err <- summary$coefficients[2, 2]

  AIC <- AIC(anova_model)
  p_value_lrt <- lmtest::lrtest(anova_model)[2,4]

  metrics <- list(
    p_value = p_value,
    std_err = std_err,
    AIC = AIC,
    p_value_lrt = p_value_lrt
  )

  result <- create_fitted_model_result(model, estimates, metrics)

  return(result)
}


