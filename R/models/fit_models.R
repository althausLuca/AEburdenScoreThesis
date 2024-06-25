#' @title Fit models
#' @description Fit models to the data
#' @param trial The trial data
#' assigns classes to the models:
#' anova_ - anova model #_ is to not conflic with the base R anova function
#' log_anova_ - log transformed anova model
#' tweedie_glm_ - tweedie model
#' quantile_regression_ - quantile regression model
#' zero_inflated_gamma_ - zero inflated gamma model
#' @return The fitted model


library(gamlss)
source("R/models/model_settings.R")

check_data <- function(trial) {
  if (!("Score" %in% names(trial))) {
    stop("Score column not found in trial data")
  }
  if (!("Group" %in% names(trial))) {
    stop("Group column not found in trial data")
  }
  trial$Group <- as.factor(trial$Group)
  trial$Group <- relevel(trial$Group, ref = "control")
  return(trial)
}



fit_tweedie_model <- function(trial, link_power = 0, var_power = 1.5, xi = var_power) {
  trial <- check_data(trial)
  if (xi == "infer") {
    print("infering xi using tweedie.profile()")
    profile_result <- tweedie.profile(trial$Score ~ trial$Group, link.power = link_power, fit.glm = F, xi.vec
      = seq(1.1, 2, by = 0.1))

    xi <- profile_result$xi.max

  }
  tweedie_model <- glm(trial$Score ~ trial$Group, family =
    tweedie(var.power = xi, link.power = link_power), control = glm.control(maxit = 100))

  #add class tweedie_glm to model
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

fit_log_anova_model <- function(trial, c = 1 , delta=c) {
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

fit_zero_inflated_gamma_model <- function(trial) {
  trial <- check_data(trial)
  model <- gamlss(Score ~ Group, sigma.formula = ~Group, nu.formula = ~Group,
                  family = ZAGA, data = trial)
  return(model) #has class gamlss
}


  get_model_fit_f <- function(model_type , args = NULL) {
  if (is.null(args)){
    args <- list()
  }

  model_fit_f <- switch(model_type,
         "anova" = fit_anova_model ,
         "log_anova" = fit_log_anova_model,
         "tweedie" = fit_tweedie_model,
         "quantile_regression" = fit_quantile_regression_model,
         "zero_inflated_gamma" = fit_zero_inflated_gamma_model
  )
  if (is.null(model_fit_f)) {
    stop("Model type not found")
  }
  return(function(trial, ...) {
    additional_args <- list(...)
    all_args <- modifyList(list(trial = trial), args)
    all_args <- modifyList(all_args, additional_args)
    do.call(model_fit_f, all_args)
  }) # returning a function
}