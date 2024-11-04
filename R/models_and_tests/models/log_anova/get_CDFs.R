get_CDFs.log_anova_model <- function(fitted_model, x,
                                     estimates = fitted_model$estimates,
                                     c = max(fitted_model$model$parameters$c, fitted_model$parameters$c),
                                     back_tranform=TRUE ) {

  stopifnot(c > 0)

  mu_treatment_log <- estimates$mu_treatment_log
  mu_control_log <- estimates$mu_control_log
  sigma_log <- max(estimates$sigma_log, estimates$sigma) # for compatibility with previous definitions

  log_x <- x
  log_x[x + c > 0] <- log(x[x + c > 0] + c)
  log_x[x + c <= 0] <- -Inf

  treatment_CDF <- pnorm(log_x, mean = mu_treatment_log, sd = sigma_log)
  control_CDF <- pnorm(log_x, mean = mu_control_log, sd = sigma_log)
  return(list(control = treatment_CDF, treatment = control_CDF , x = log_x))
}