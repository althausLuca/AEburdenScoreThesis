get_CDFs.anova_model <- function(fitted_model , x , estimates = fitted_model$estimates) {
  mu_control <-  estimates$mu_control
  mu_treatment <- estimates$mu_treatment
  sd <- estimates$sigma
  treatment_CDF <- pnorm(x, mean = mu_control, sd = sd)
  control_CDF <- pnorm(x, mean = mu_treatment, sd = sd)
  return(list(control = treatment_CDF, treatment = control_CDF))
}