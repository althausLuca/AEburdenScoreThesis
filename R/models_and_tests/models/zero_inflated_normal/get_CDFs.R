get_CDFs.zero_inflated_normal <- function(fitted_model, x, estimates = fitted_model$estimates) {

  mu_treatment <- estimates$mu_treatment
  mu_control <- estimates$mu_control
  nu_treatment <- estimates$nu_treatment
  nu_control <- estimates$nu_control
  sigma_control <- ifelse(is.null(estimates$sigma), estimates$sigma_control, estimates$sigma)
  sigma_treatment <- ifelse(is.null(estimates$sigma), estimates$sigma_treatment, estimates$sigma)

  get_CDF <- function(x, mu, sigma, nu) {
     #' Calculate the CDF of the zero-inflated normal distribution
     #' x - the value/values at which to calculate the CDF
     #' mu - the mean of the normal distribution of the non-zero values
     #' sigma - the standard deviation of the normal distribution of the non-zero values
     #' nu - the probability of obtaining a zero
    cdf <- nu * (x >= 0) + (1 - nu) * pnorm(x, mean = mu, sd = sigma)
    return(cdf)
  }

  treatment_CDF <- get_CDF(x, mu_treatment, sigma_treatment, nu_treatment)
  control_CDF <- get_CDF(x, mu_control, sigma_control, nu_control)

  return(list(control = control_CDF, treatment = treatment_CDF))
}
