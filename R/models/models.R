# define models with their parameters

ANOVA <- function() {
  model <- list()
  class(model) <- "anova_model"
  model$repr <- "anova"
  model$name <- "ANOVA"
  return(model)
}

LOG_ANOVA <- function(delta = 0.001) {
  model <- list()
  class(model) <- "log_anova_model"
  model$delta <- delta
  model$repr <- paste0("log_anova_c_", delta)
  model$name <- "Log-ANOVA"
  return(model)
}

TWEEDIE_REGRESSION <- function(var_power = 1.5, link_power = 0) {
  model <- list()
  class(model) <- "tweedie_glm_model"
  model$var_power <- var_power
  model$link_power <- link_power
  model$repr <- paste0("tweedie_var_power_", var_power, "_link_power_", link_power)
  model$name <- "Tweedie"
  return(model)
}

QUANTILE_REGRESSION <- function(tau = 0.5) {
  model <- list()
  class(model) <- "quantile_regression_model"
  model$tau <- tau
  model$repr <- paste0("quantile_regression_tau_", tau)
  model$name <- ifelse(tau == 0.5, "Median Regression", "Quantile Regression")
  return(model)
}

ZERO_INFLATED_GAMMA <- function(sigma_per_group = FALSE) {
  model <- list()
  class(model) <- "zero_inflated_gamma_model"
  model$repr <- ifelse(sigma_per_group, paste0("zero_inflated_gamma_sigma_per_group") , "zero_inflated_gamma")
  model$name <- "Zero-Inflated Gamma"
  model$sigma_per_group <- sigma_per_group
  return(model)
}

WILCOXON_TEST <- function() {
  model <- list()
  class(model) <- "wilcoxon_test"
  model$repr <- "wilcoxon"
  model$name <- "Wilcoxon Test"
  return(model)
}

PERMUTATION_TEST <- function(n_permutations = 10000) {
  model <- list()
  class(model) <- "permutation_test"
  model$n_permutations <- n_permutations
  model$repr <- paste0("permutation_test_nper_", n_permutations)
  model$name <- "Permutation Test"
  return(model)
}

list(ANOVA, LOG_ANOVA, TWEEDIE_REGRESSION, QUANTILE_REGRESSION, ZERO_INFLATED_GAMMA, WILCOXON_TEST, PERMUTATION_TEST)