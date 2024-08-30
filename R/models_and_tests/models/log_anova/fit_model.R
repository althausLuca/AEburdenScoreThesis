fit_model.log_anova_model <- function(model, trial, c = model$parameters$c) {
  trial <- check_data(trial)

  trial$log_score <- log(trial$Score + c)
  log_anova_model <- lm(trial$log_score ~ trial$Group)

  summary <- summary(log_anova_model)

  ## Extract Coefficients
  sigma<- summary$sigma

  mu_control_log <- coef(log_anova_model)[1]
  mu_treatment_log <- sum(coef(log_anova_model))

  mu_control <- exp(mu_control_log) - c
  mu_treatment <- exp(mu_treatment_log) - c

  estimates <- list(
    mu_control = mu_control,
    mu_treatment = mu_treatment,
    sigma = sigma
  )

  ## Extract treatment effect p_value
  p_value <- summary$coefficients[2, 4]
  std_err <- summary$coefficients[2, 2]


  get_CDFs <- function(x) {
    log_x <- x
    log_x[x + c > 0] <- log(x[x + c > 0] + c)
    log_x[x + c <= 0] <- -Inf
    treatment_CDF <- pnorm(log_x, mean = mu_treatment_log, sd = sigma)
    control_CDF <- pnorm(log_x, mean = mu_control_log, sd = sigma)
    return(list(control = treatment_CDF, treatment = control_CDF))
  }

  result <- list(
    model = log_anova_model,
    estimates = estimates,
    p_value = p_value,
    std_err = std_err,
    get_CDFs = get_CDFs,
    c = c,
    AIC = AIC(log_anova_model)
  )
}