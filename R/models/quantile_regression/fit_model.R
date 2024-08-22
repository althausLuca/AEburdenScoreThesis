source("R/models/model_helpers.R")
library(quantreg)

fit_quantile_regression_model <- function(trial, tau = 0.5) {
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

  ker.p_value <- tryCatch({
    summary(quantile_regression_model, se = "ker")$coefficients[2, "Pr(>|t|)"]
  }, error = function(e) {
    NA
  })
  nid.p_value <- tryCatch({
    summary(quantile_regression_model, se = "nid")$coefficients[2, "Pr(>|t|)"]
  }, error = function(e) {
    NA
  })
  boot.p_value <- tryCatch({
    summary(quantile_regression_model, se = "boot")$coefficients[2, "Pr(>|t|)"]
  }, error = function(e) {
    NA
  })

  results <- list(
    model = quantile_regression_model,
    AIC = AIC,
    estimates = estimates,
    tau = tau,
    ker_p_value = ker.p_value,
    nid_p_value = nid.p_value,
    boot_p_value = boot.p_value,
    p_value = ker.p_value
  )

}
