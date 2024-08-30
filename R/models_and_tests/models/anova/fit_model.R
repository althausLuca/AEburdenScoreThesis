fit_model.anova_model <- function(model, trial) {
  trial <- check_data(trial)

  ##model fitting
  anova_model <- lm(trial$Score ~ trial$Group)
  class(anova_model) <- c("anova_", class(anova_model))

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


  get_CDFs <- function(x) {
    treatment_CDF <- pnorm(x, mean = mu_treatment, sd = sigma)
    control_CDF <- pnorm(x, mean = mu_control, sd = sigma)
    return(list(control = treatment_CDF, treatment = control_CDF))
  }

  result <- list(
    model = anova_model,
    estimates = estimates,
    p_value = p_value,
    std_err = std_err,
    AIC = AIC,
    get_CDFs = get_CDFs
  )

  return(result)
}


