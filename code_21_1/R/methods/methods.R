library(quantreg)
library(statmod)
library(tweedie)

#' Define a function to run standard linear regression, Tweedie regression, and Quantile regression
#' Parameters:
#' - score_data: A data frame with two columns scores and groups
#' - link.power: link.power for the tweedy regression (default 0)
#' - var.power:  var.power for the tweedy regression (default 1.5)
#' - delta: A small number to add to the scores to avoid log(0) in log anova model
#' Returns a list containing results from different models i.e., lm , tweedie, quantile_regression, log_anova, wilcoxon test
run_models <- function(score_data, link.power = 0, var.power = 1.5, delta = 1) {
  results <- list()

  results$lm <- run_lm(score_data)
  ## Log Anova model
  results$log_anova <- run_anova(score_data, delta = delta)

  ## Tweedie model
  results$tweedy <- run_tweedie(score_data, var.power = var.power, link.power = link.power)

  ## Quantile regression
  results$quantile_regression <- run_qauntile_regression(score_data)

  ## Rank test
  wilcoxon_result <- wilcox.test(scores ~ groups, data = score_data)
  p_value <- wilcoxon_result$p.value
  results$wilcoxon <- list("model" = "wilcoxon",
                           "p_value" = p_value)

  return(results)
}

#' Define a function to run log anova model
#' Parameters:
#' - score_data: A data frame with two columns scores and groups
#' Returns a list containing results from a linear model
run_lm <- function(score_data) {
  lm_model <- lm(scores ~ groups, data = score_data)
  summary <- summary(lm_model)
  p_value <- summary$coefficients[2, 4]
  std_err <- summary$coefficients[2, 2]
  t_value <- summary$coefficients[2, 3]
  estimate <- summary$coefficients[2, 1]

  AIC <- AIC(lm_model)
  results <- list("model" = "lm",
                     "AIC" = AIC,
                     "p_value" = p_value,
                     "std_err" = std_err,
                     "t_value" = t_value,
                     "estimate" = estimate)
  return(results)
}

#' Define a function to run log anova model
#' Parameters:
#' - score_data: A data frame with two columns scores and groups
#' - delta: A small number to add to the scores to avoid log(0) in log anova model
#' Returns a list containing results from log anova model
run_anova <- function(score_data, delta = 1) {
  ## Log Anova model
  score_data.ln <- score_data
  score_data.ln$scores <- log(score_data.ln$scores + delta)
  anova_result.ln <- aov(scores ~ groups, data = score_data.ln)
  anova_summary <- summary(anova_result.ln)
  p_value <- anova_summary[[1]]$`Pr(>F)`[1]
  results <- list("model" = "log_anova",
                  "p_value" = p_value)

  return(results)
}

#' Define a function to run Tweedie regression
#' Parameters:
#' - score_data: A data frame with two columns scores and groups
#' - link.power: link.power for the tweedy regression (default 0)
#' - var.power:  var.power for the tweedy regression (default 1.5)
#' Returns a list containing results from Tweedie regression
run_tweedie <- function(score_data, var.power = 1.5, link.power = 0) {
  tweedie_model <- glm(score_data$scores ~ score_data$groups, family =
    tweedie(var.power = var.power, link.power = link.power), data = score_data, control = glm.control(maxit = 100))

  AIC <- AICtweedie(tweedie_model, dispersion = 1)
  # get the p-value for the treatment group coefficient and the standard error and t-value
  summary_tweedy <- summary(tweedie_model)
  p_value <- summary_tweedy$coefficients[2, 4]
  std_err <- summary_tweedy$coefficients[2, 2]
  t_value <- summary_tweedy$coefficients[2, 3]
  estimate <- summary_tweedy$coefficients[2, 1]

  results <- list("model" = "tweedie",
                  "AIC" = AIC,
                  "p_value" = p_value,
                  "std_err" = std_err,
                  "t_value" = t_value,
                  "estimate" = estimate,
                  "link.power" = link.power,
                  "var.power" = var.power)

  return(results)
}

run_qauntile_regression <- function(score_data) {
  #https://cran.r-project.org/web/packages/quantreg/vignettes/rq.pdf
  quantile_regression_model <- rq(score_data$scores ~ score_data$groups)
  AIC <- AIC(quantile_regression_model)[1]
  # get the p-value for the treatment group coefficient and the standard error and t-value

  coefficient <- summary(quantile_regression_model)$coefficients[2, 1]
  lower_bound <- summary(quantile_regression_model)$coefficients[2, 2]
  upper_bound <- summary(quantile_regression_model)$coefficients[2, 3]

  sid_quantille_summary <- summary(quantile_regression_model, se = "nid") #or ker
  p_value <- sid_quantille_summary$coefficients[1, "Pr(>|t|)"]
  quantile_std_err <- sid_quantille_summary$coefficients[1, "Std. Error"]
  results <- list("model" = "quantile_regression",
                  "AIC" = AIC,
                  "p_value" = p_value,
                  "estimate" = coefficient,
                  "lower_bound" = lower_bound,
                  "upper_bound" = upper_bound,
                  "std_err" = quantile_std_err)

  return(results)
}