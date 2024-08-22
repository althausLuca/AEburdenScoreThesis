#' This file combines  all the model running functions
#' Models are:
#' - Linear regression
#' - Tweedie regression
#' - Quantile regression
#' - Log-ANOVA

source("R/models/models.R")

DEFAULT_MODELS <- list(
  ANOVA(),
  LOG_ANOVA(c = 0.001),
  LOG_ANOVA(c = 1),
  LOG_ANOVA(c = 10000),
  TWEEDIE_REGRESSION(xi = 1.2),
  QUANTILE_REGRESSION(),
  WILCOXON_TEST(),
  PERMUTATION_TEST(),
  ZERO_INFLATED_GAMMA(),
  ZERO_INFLATED_LOGNORMAL(),
  ZERO_INFLATED_TTEST()
)

#' Define a function to run standard linear regression, Tweedie regression, and Quantile regression
#' Parameters:
#' - trial: A data frame with two columns scores and group
#' - models: A list of models to run
#' Returns:
#' - A list of results
#'  - trial: The trial data
#' - model results: A list of results for each model
#'  - p_value: The p-value of the model
#' - estimates: The estimates of the model
#' - test results: The (two sided) p-value of the test
run_models <- function(trial, models = DEFAULT_MODELS) {
  trial <- check_data(trial)

  result <- list(trial = trial)

  for (model in models) {
    model_representation <- model$repr
    print(model_representation)
    if (inherits(model, "model")) {
      model_fit <- model$fit(trial)
      p_value <- model_fit$p_value
      estimates <- model_fit$estimates
      result$models[[model_representation]] <- list(p_value = p_value, estimates = estimates)
    }
    else if (inherits(model, "test")) {
      p_value <- model$test(trial)$p.value
      result[[model_representation]] <- p_value
    }
  }
  return(result)
}


#' Define a function to run models from trial data
#' Parameters:
#' - trial_data: A list of data frames with two columns scores and group
#' - trial_pre_procesing: A function trial_df -> trial_df  to preprocess the trial data , e.g subsetting
#' - ... : Additional parameters to pass to run_models
run_models_from_trial_data <- function(trial_data, models = DEFAULT_MODELS,
                                       trial_pre_procesing = function(x) { return(x) }, use_parallel = FALSE,
                                       ...) {


  model_results <- trial_data$apply_to_each(
    function(trial) {
      trial <- trial_pre_procesing(trial)
      return(run_models(trial, models = models))
    },use_parallel = use_parallel)


  return(model_results)

}






