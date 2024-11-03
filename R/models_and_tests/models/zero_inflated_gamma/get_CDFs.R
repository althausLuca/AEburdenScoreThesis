get_CDFs.zero_inflated_gamma_model <- function(fitted_model, x, estimates = fitted_model$estimates) {

  sigma_control <- ifelse(is.null(estimates$sigma), estimates$sigma_control, estimates$sigma)
  sigma_treatment <- ifelse(is.null(estimates$sigma), estimates$sigma_treatment, estimates$sigma)

  control_CDF <- pZAGA(x, mu = estimates$mu_control, sigma = sigma_control, nu = estimates$nu_control)
  treatment_CDF <- pZAGA(x, mu = estimates$mu_treatment, sigma = sigma_treatment, nu = estimates$nu_treatment)

  return(list(control = control_CDF, treatment = treatment_CDF))
}
