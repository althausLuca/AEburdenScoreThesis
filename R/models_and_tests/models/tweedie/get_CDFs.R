get_CDFs.tweedie_glm_model <- function(fitted_model, x, estimates = fitted_model$estimates) {

  mu_treatment <- estimates$mu_treatment
  mu_control <- estimates$mu_control
  phi <- estimates$phi
  xi <- estimates$xi

  treatment_CDF <- ptweedie(x, mu = mu_treatment, phi = phi, power = xi)
  control_CDF <- ptweedie(x, mu = mu_control, phi = phi, power = xi)

  return(list(control = treatment_CDF, treatment = control_CDF))
}