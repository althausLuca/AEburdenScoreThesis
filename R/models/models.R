# define models
ANOVA <- function() {
  model <- list()
  class(model) <- c("anova_model", "model")
  model$parameters <- list()
  model$repr <- "anova"
  model$name <- "ANOVA"
  model$fit_function <- source("R/models/anova/fit_model.R")$value
  model$fit <- function(trial) model$fit_function(trial)
  return(model)
}

LOG_ANOVA <- function(c = 0.001) {
  model <- list()
  class(model) <- c("log_anova_model", "model")
  model$parameters <- list(c = c)
  model$repr <- paste0("log_anova_c_", c)
  model$name <- "Log-ANOVA"
  model$fit_function <- source("R/models/log_anova/fit_model.R")$value
  model$fit <- function(trial) model$fit_function(trial, c = c)
  return(model)
}

TWEEDIE_REGRESSION <- function(var_power = 1.5, link_power = 0, xi = var_power) {
  model <- list()
  class(model) <- c("tweedie_glm_model", "model")
  model$parameters <- list(link_power = link_power, xi = xi)
  model$repr <- paste0("tweedie_var_power_", var_power, "_link_power_", link_power)
  model$name <- "Tweedie"
  model$fit_function <- source("R/models/tweedie/fit_model.R")$value
  model$fit <- function(trial) model$fit_function(trial, link_power = link_power, xi = xi)
  return(model)
}

QUANTILE_REGRESSION <- function(tau = 0.5) {
  model <- list()
  class(model) <- c("quantile_regression_model", "model")
  model$parameters <- list(tau = tau)
  model$repr <- paste0("quantile_regression_tau_", tau)
  model$name <- ifelse(tau == 0.5, "Median Regression", "Quantile Regression")
  model$fit_function <- source("R/models/quantile_regression/fit_model.R")$value
  model$fit <- function(trial) model$fit_function(trial, tau = tau)
  return(model)
}


ZERO_INFLATED_GAMMA <- function(sigma_per_group = FALSE) {
  model <- list()
  class(model) <- c("zero_inflated_gamma_model", "model")
  model$parameters <- list(sigma_per_group = sigma_per_group)
  model$repr <- ifelse(sigma_per_group, paste0("zero_inflated_gamma_sigma_per_group"), "zero_inflated_gamma")
  model$name <- "Zero-Inflated Gamma"

  source("R/models/zero_inflated_gamma/fit_model.R", local = TRUE)
  model$fit <- function(trial) fit_zero_inflated_gamma_model(trial, sigma_per_group = sigma_per_group)
  return(model)
}

ZERO_INFLATED_LOGNORMAL <- function(sigma_per_group = FALSE) {
  model <- list()
  class(model) <- c("zero_inflated_lognormal", "model")
  model$parameters <- list(sigma_per_group = sigma_per_group)
  model$repr <- ifelse(sigma_per_group, paste0("zero_inflated_lognormal_per_group"), "zero_inflated_lognormal")
  model$name <- "Zero-Inflated Lognormal"
  model$sigma_per_group <- sigma_per_group
  source("R/models/zero_inflated_lognormal/fit_model.R",local=TRUE)
  print(model$fit_function)
  model$fit <- function(trial) fit_zero_inflated_lognormal_model(trial, sigma_per_group = sigma_per_group)
  return(model)
}

# define tests
WILCOXON_TEST <- function() {
  model <- list()
  class(model) <- c("wilcoxon_test", "test")
  model$repr <- "wilcoxon"
  model$name <- "Wilcoxon Test"
  model$test <- function(trial) wilcox.test(trial$Score ~ trial$Group)
  return(model)
}

PERMUTATION_TEST <- function(n_permutations = 10000) {
  source("R/stat_tests/permutation_test.R")
  model <- list()
  class(model) <- c("permutation_test", "test")
  model$n_permutations <- n_permutations
  model$repr <- paste0("permutation_test_nper_", n_permutations)
  model$name <- "Permutation Test"
  model$test <- function(trial) mean_permutation_test(trial, n_permutations = n_permutations)
  return(model)
}

ZERO_INFLATED_TTEST <- function() {
  source("R/stat_tests/zero_inflated_test.R")
  model <- list()
  class(model) <- c("zero_inflate_ttest", "test")
  model$repr <- "zero_inflate_ttest"
  model$name <- "Zero-Inflated T-Test"
  model$test <- two_part_test
  return(model)
}

ZERO_INFLATED_WILCOXON_TEST <- function() {
  source("R/stat_tests/zero_inflated_test.R")
  model <- list()
  class(model) <- c("zero_inflate_wilcoxon", "test")
  model$repr <- "zero_inflate_wilcoxon"
  model$name <- "Zero-Inflated Wilcoxon Test"
  model$test <- function(trial) two_part_test(trial, test = "wilcoxon")
  return(model)
}





