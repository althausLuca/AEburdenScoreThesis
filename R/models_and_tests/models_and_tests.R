# define models
ANOVA <- function() {
  model <- list()
  class(model) <- c("anova_model", "model")
  model$parameters <- list()
  model$repr <- "anova"
  model$name <- "ANOVA"
  return(model)
}

LOG_ANOVA <- function(c = 0.001) {
  model <- list()
  class(model) <- c("log_anova_model", "model")
  model$parameters <- list(c = c)
  model$repr <- paste0("log_anova_c_", c)
  model$name <- "Log-ANOVA"
  return(model)
}

TWEEDIE_REGRESSION <- function(var_power = 1.5, link_power = 0, xi = var_power , use_mle = F) {
  model <- list()
  class(model) <- c("tweedie_glm_model", "model")
  model$parameters <- list(link_power = link_power, xi = xi , use_mle = use_mle)
  base_repr <- paste0("tweedie_var_power_", xi, "_link_power_", link_power)
  model$repr <- ifelse(use_mle, paste0(base_repr, "_mle"), base_repr)
  model$name <- "Tweedie"
  return(model)
}

QUANTILE_REGRESSION <- function(tau = 0.5, summary_method = "xy") {
  model <- list()
  class(model) <- c("quantile_regression_model", "model")
  model$parameters <- list(tau = tau, summary_method = summary_method)
  model$repr <- paste0("quantile_regression_tau_", tau, "_", summary_method)
  model$name <- ifelse(tau == 0.5, "Median Regression", "Quantile Regression")
  return(model)
}


ZERO_INFLATED_GAMMA <- function(sigma_per_group = FALSE) {
  model <- list()
  class(model) <- c("zero_inflated_gamma_model", "model")
  model$parameters <- list(sigma_per_group = sigma_per_group)
  model$repr <- ifelse(sigma_per_group, paste0("zero_inflated_gamma_sigma_per_group"), "zero_inflated_gamma")
  model$name <- "Zero-Inflated Gamma"
  return(model)
}

ZERO_INFLATED_LOGNORMAL <- function(sigma_per_group = FALSE) {
  model <- list()
  class(model) <- c("zero_inflated_lognormal", "model")
  model$parameters <- list(sigma_per_group = sigma_per_group)
  model$repr <- ifelse(sigma_per_group, paste0("zero_inflated_lognormal_per_group"), "zero_inflated_lognormal")
  model$name <- "Zero-Inflated Lognormal"
  model$sigma_per_group <- sigma_per_group
  return(model)
}

# STATISTICAL TESTS
WILCOXON_TEST <- function() {
  model <- list()
  class(model) <- c("wilcoxon_test", "test")
  model$repr <- "wilcoxon"
  model$name <- "Wilcoxon Test"
  return(model)
}

PERMUTATION_TEST <- function(n_permutations = 10000) {
  model <- list()
  class(model) <- c("permutation_test", "test")
  model$parameters <- list(n_permutations = n_permutations)
  model$repr <- paste0("permutation_test_nper_", n_permutations)
  model$name <- "Permutation Test"
  return(model)
}

TWO_PART_T_TEST <- function(use_welch = FALSE) {
  model <- list()
  class(model) <- c("two_part_t_test", "test")
  model$parameters <- list(use_welch = use_welch)
  model$repr <- ifelse(!use_welch , "two_part_ttest" , "two_part_welch")
  model$name <- "Zero-Inflated T-Test"
  return(model)
}

TWO_PART_WILCOXON_TEST <- function() {
  model <- list()
  class(model) <- c("two_part_wilcoxon", "test")
  model$parameters <- list()
  model$repr <- "two_part_wilcoxon"
  model$name <- "Zero-Inflated Wilcoxon Test"
  return(model)
}

ZERO_INFLATED_NORMAL <- function(sigma_per_group = FALSE) {
  model <- list()
  class(model) <- c("zero_inflated_normal", "model")
  model$parameters <- list(sigma_per_group = sigma_per_group)
  model$repr <- ifelse(!sigma_per_group,"zero_inflated_normal", "zero_inflated_normal_per_group")
  model$name <- "Zero-Inflated Normal"
  return(model)
}

source("R/models_and_tests/init_models_and_tests.R")