#' @title Fit models
#' @description Fit models to the data
#' @param trial The trial data
#' assigns classes to the models:
#' anova_ - anova model #_ is to not conflic with the base R anova function
#' log_anova_ - log transformed anova model
#' tweedie_glm_ - tweedie model
#' quantile_regression_ - quantile regression model
#' zero_inflated_gamma_ - zero inflated gamma model

library(gamlss)
library(quantreg)
library(tweedie)
library(statmod)
source("R/models/model_settings.R")
source("R/models/model_helpers.R")
source("../stat_tests/permutation_test.R")

fit_tweedie_model <- function(trial, link_power = 0, var_power = 1.5, xi = var_power) {
  trial <- check_data(trial)
  if (xi == "infer") {
    print("infering xi using tweedie.profile()")
    profile_result <- tweedie.profile(trial$Score ~ trial$Group, link.power = link_power, fit.glm = F, xi.vec
      = seq(1.1, 2, by = 0.1))

    xi <- profile_result$xi.max

  }
  tweedie_model <- glm(trial$Score ~ trial$Group,
                        family = tweedie(var.power = xi, link.power = link_power), control = glm.control(maxit = 100))

  #add class tweedie_glm_ to model
  class(tweedie_model) <- c("tweedie_glm_", class(tweedie_model))
  tweedie_model$xi <- xi # add xi to the model

  return(tweedie_model)
}

fit_anova_model <- function(trial) {
  trial <- check_data(trial)
  anova_model <- lm(trial$Score ~ trial$Group)
  class(anova_model) <- c("anova_", class(anova_model))
  return(anova_model)
}

fit_log_anova_model <- function(trial, c = 1, delta = c) {
  trial <- check_data(trial)
  trial$log_score <- log(trial$Score + delta)
  log_anova_model <- lm(trial$log_score ~ trial$Group)

  class(log_anova_model) <- c("log_anova_", "anova_", class(log_anova_model))

  log_anova_model$c <- delta
  return(log_anova_model)
}

fit_quantile_regression_model <- function(trial, tau = 0.5) {
  trial <- check_data(trial)
  quantile_regression_model <- rq(trial$Score ~ trial$Group, tau = 0.5)
  quantile_regression_model$tau <- tau
  class(quantile_regression_model) <- c("quantile_regression_", class(quantile_regression_model))
  return(quantile_regression_model)
}


## Tests
wilcoxon_test <- function(trial) {
  trial <- check_data(trial)
  wilcox_test <- wilcox.test(trial$Score ~ trial$Group)

  class(wilcox_test) <- c("wilcoxon_test", class(wilcox_test))
  return(wilcox_test)
}

permutation_test <- function(trial, n_permutations = 10000) {
  trial <- check_data(trial)

  permutation_test <- mean_permutation_test(trial, n_permutations)

  class(permutation_test) <- c("permutation_test", class(permutation_test))

  return(permutation_test)
}


#' @title Fit model
#' @description Fit a model to the data
#' @param model The model to fit to the data, this is a list of model settings and the model containing the class
#' @param trial The trial data
#' @return The fitted model
fit_model <- function(model, trial, ...) {
  UseMethod("fit_model")
}

#' @title Log anova model
fit_model.log_anova_model <- function(model, trial, ...) {
  fit_log_anova_model(trial, delta = model$delta)
}

#' @title Anova model
fit_model.anova_model <- function(model, trial, ...) {
  fit_anova_model(trial)
}

#' @title Tweedie model
fit_model.tweedie_glm_model <- function(model, trial, ...) {
  fit_tweedie_model(trial, link_power = model$link_power, var_power = model$var_power)
}

#' @title Quantile regression model
fit_model.quantile_regression_model <- function(model, trial, ...) {
  fit_quantile_regression_model(trial, tau = model$tau)
}

#' @title Zero inflated gamma model
fit_model.zero_inflated_gamma_model <- function(model, trial, ...) {
  fit_zero_inflated_gamma_model(trial, sigma_per_group = model$sigma_per_group)
}

#' @title Wilcoxon test
fit_model.wilcoxon_test <- function(model, trial, ...) {
  wilcoxon_test(trial)
}

#' @title Permutation test
fit_model.permutation_test <- function(model, trial, ...) {
  permutation_test(trial, n_permutations = model$n_permutations)
}