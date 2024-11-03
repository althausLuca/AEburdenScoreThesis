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

# we do not store the trials for each model and test again
create_fitted_model_result <- function(model, estimates, metrics , ...) {
    results <- list(
        model = model,
        estimates = estimates,
        metrics = metrics,
        ...
    )
    class(results) <- c(class(model),"model_result")
    return(results)
}

create_test_result <- function(test, p_value, ...) {
    results <- list(
        test = test,
        p_value = p_value,
        ...
    )
    class(results) <- c(class(test),"test_result")
    return(results)
}

#' @title set the force computation flag for a model or test
#' @param model model or test object to force the model_computer to recompute the results
#' @return model or test object with force_computation flag set to TRUE
force_computation <- function(model) {
  model$force_computation <- TRUE
  return(model)
}


