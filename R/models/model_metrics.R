library(tweedie)
library(statmod)
library(quantreg)


# Define a generic function
extract_metrics <- function(model, ...) {
  UseMethod("extract_metrics")
}

extract_metrics.anova_ <- function(lm_model, ...) {
  summary <- summary(lm_model)
  p_value <- summary$coefficients[2, 4]
  std_err <- summary$coefficients[2, 2]
  t_value <- summary$coefficients[2, 3]
  estimate <- summary$coefficients[2, 1]

  AIC <- AIC(lm_model)

  results <- list("AIC" = AIC,
                  "p_value" = p_value,
                  "std_err" = std_err,
                  "t_value" = t_value,
                  "estimate" = estimate)
  return(results)

}

extract_metrics.gamlss <- function(gamlss_model, ...) {
  model_summary <- summary(gamlss_model)

  mu_p_val <- model_summary[2, 4]
  sigma_p_val <- model_summary[4, 4]
  nu_p_pal <- model_summary[6, 4]

  AIC_ <- AIC(gamlss_model)
  result <- list("mu_p_value" = mu_p_val, "sigma_p_value" = sigma_p_val, "nu_p_value" = nu_p_pal, "AIC" = AIC_)
  return(result)
}


extract_metrics.glm <- function(tweedie_model, ...) {
  AIC <- AICtweedie(tweedie_model, dispersion = 1)
  # get the p-value for the treatment group coefficient and the standard error and t-value
  summary_tweedy <- summary(tweedie_model)
  p_value <- summary_tweedy$coefficients[2, 4]
  std_err <- summary_tweedy$coefficients[2, 2]
  t_value <- summary_tweedy$coefficients[2, 3]
  estimate <- summary_tweedy$coefficients[2, 1]

  results <- list("AIC" = AIC,
                  "p_value" = p_value,
                  "std_err" = std_err,
                  "t_value" = t_value,
                  "estimate" = estimate)
}

extract_metrics.permutation_test <- function(permutation_test_results, ...) {
  p_value <- permutation_test_results$p_value
  p_value_abs <- permutation_test_results$p_value_abs
  p_value_l <- permutation_test_results$p_value_l
  p_value_g <- permutation_test_results$p_value_g

  results <- list("p_value" = p_value,
                  "p_value_abs" = p_value_abs,
                  "p_value_l" = p_value_l,
                  "p_value_g" = p_value_g)
  return(results)
}

extract_metrics.wilcoxon_test <- function(wilcox_test_results, ...) {
  p_value <- wilcox_test_results$p.value
  results <- list("p_value" = p_value)
  return(results)
}


extract_metrics.quantile_regression_ <- function(quantile_regression_model, ...) {
  summary <- summary(quantile_regression_model)
  estimate <- summary$coefficients[2, 1]

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

  results <- list("AIC" = AIC,
                  "p_value" = ker.p_value,
                  "ker_p_value" = ker.p_value,
                  "nid_p_value" = nid.p_value,
                  "boot_p_value" = boot.p_value,
                  "estimate" = estimate)
  return(results)
}

