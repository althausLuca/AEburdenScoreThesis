get_CDFs.zero_inflated_lognormal <- function(fitted_model, x, estimates = fitted_model$estimates) {
  sigma_control <- ifelse(is.null(estimates$sigma), estimates$sigma_control, estimates$sigma)
  meanlog_control <- log(estimates$mu_control^2 / (estimates$mu_control^2 + sigma_control^2)^0.5)
  sdlog_control <- log(1 + sigma_control^2 / estimates$mu_control^2)^0.5

  sigma_treatment <- ifelse(is.null(estimates$sigma), estimates$sigma_treatment, estimates$sigma)
  meanlog_treatment <- log(estimates$mu_treatment^2 / (estimates$mu_treatment^2 + sigma_treatment^2)^0.5)
  sdlog_treatment <- log(1 + sigma_treatment^2 / estimates$mu_treatment^2)^0.5

  control_CDFs <- plnorm(x, meanlog = meanlog_control,
                         sdlog = sdlog_control) * (1 - estimates$nu_control) + estimates$nu_control * (x >= 0)
  treatment_CDFs <- plnorm(x, meanlog = meanlog_treatment,
                           sdlog = sdlog_treatment) * (1 - estimates$nu_treatment) + estimates$nu_treatment * (x >= 0)

  return(list(control = control_CDFs, treatment = treatment_CDFs))
}
