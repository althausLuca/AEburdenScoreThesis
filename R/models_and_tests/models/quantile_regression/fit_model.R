library(quantreg)

fit_model.quantile_regression_model <- function(model, trial, tau = model$parameters$tau, summary_method = model$parameters$summary_method) {
  trial <- check_data(trial)
  quantile_regression_model <- rq(trial$Score ~ trial$Group, tau = tau)
  quantile_regression_model$tau <- tau

  mu_control <- coef(quantile_regression_model)[1]
  mu_treatment <- sum(coef(quantile_regression_model))

  estimates <- list(
    mu_control = mu_control,
    mu_treatment = mu_treatment
  )

  AIC <- AIC(quantile_regression_model)

  # ker.p_value <- tryCatch({
  #   summary(quantile_regression_model, se = "ker")$coefficients[2, "Pr(>|t|)"]
  # }, error = function(e) {
  #   NA
  # })
  # nid.p_value <- tryCatch({
  #   summary(quantile_regression_model, se = "nid")$coefficients[2, "Pr(>|t|)"]
  # }, error = function(e) {
  #   NA
  # })
  boot_xy.p_value <- tryCatch({
    summary(quantile_regression_model, se = "boot", bsmethod = "xy", R=500)$coefficients[2, "Pr(>|t|)"]
  }, error = function(e) {
    NA
  })
  # boot_pwy.p_value <- tryCatch({
  #   summary(quantile_regression_model, se = "boot", bsmethod = "pwy")$coefficients[2, "Pr(>|t|)"]
  # }, error = function(e) {
  #   NA
  # })
  #
  # boot_mcmb.p_value <- tryCatch({
  #   summary(quantile_regression_model, se = "boot", bsmethod = "mcmb")$coefficients[2, "Pr(>|t|)"]
  # }, error = function(e) {
  #   NA
  # })

  p_value <- switch(summary_method,
                    "ker" = ker.p_value,
                    "nid" = nid.p_value,
                    "boot_xy" = boot_xy.p_value,
                    "xy" = boot_xy.p_value,
                    "boot_pwy" = boot_pwy.p_value,
                    "pwy" = boot_pwy.p_value,
                    "boot_mcmb" = boot_mcmb.p_value,
                    "mcmb" = boot_mcmb.p_value,
                    stop("Invalid summary method")
  )

  metrics <- list(
    AIC = AIC,
    p_value = p_value
  )

  p_value_methods <- list(
    # ker_p_value = ker.p_value,
    # nid_p_value = nid.p_value,
    boot_xy.p_value = boot_xy.p_value
    # boot_pwy.p_value = boot_pwy.p_value,
    # boot_mcmb.p_value = boot_mcmb.p_value
  )

  results <- create_fitted_model_result(model, estimates, metrics, p_value_methods= p_value_methods)
  return(results)
}
