library(data.table)

source("R/data_generation/trial_data.R")

#' @title model_computer
#' @description A object to store and manage model results, allows adding model results to existing results form other models
#' and saving/loading to/from file. Also provides a function to plot p-values of models and stores the trial data the models are applied on.
#'
#' @param trial_data A trial_data object
#' @param name A string specifying the name of the model computer
#' @param path A string specifying the path to save the model computer
#' @param check_exists A boolean specifying whether to check if the model computer already exists
#' (default = TRUE and will load the model computer if it exists instead of creating a new one)
#' @return A model_computer object
init_model_computer <- function(trial_data, name = "model_computer", path = "results/model_results/", check_exists = TRUE) {
  file_path <- paste0(gsub("/$", "", path), "/", name, ".RData")
  file_path <- gsub("//", "/", file_path)
  file_path <- gsub(".RData.RData", ".RData", file_path)

  if (check_exists) {
    model_computer <- load_model_computer(file_path)
    if (!isFALSE(model_computer)) {
      if(check_if_trial_data_is_equal(model_computer$trial_data, trial_data)){
        print(paste0("Loaded existing results from: ", file_path))
        return(model_computer)
      }
      print(paste0("Trial data is different ... reinitalizing model computer"))
    }
  }

  result <- new.env()
  name <- name
  class(result) <- "model_computer"

  result$trial_data <- trial_data
  result$models <- list()
  result$model_metrics <- list()

  result$model_estimates <- list()

  result$name <- name
  result$file_path <- file_path

  return(result)
}


save.model_computer <- function(model_computer) {
  dir.create(dirname(model_computer$file_path), recursive = TRUE, showWarnings = FALSE)
  base::save(model_computer, file = model_computer$file_path)
}


load_model_computer <- function(file_path) {
  if (!file.exists(file_path)) {
    return(FALSE)
  }
  base::load(file_path, env = tmp <- new.env())
  if (is.null(tmp$model_computer) | !inherits(tmp$model_computer, "model_computer")) {
    stop("file does not contain a model_computer object")
  }
  return(tmp$model_computer)
}

add_model <- function(model_computer, model_or_test, save = TRUE, recompute = FALSE) {

  if (isTRUE(model_or_test$force_computation)) {
    drop_model(model_computer, model_or_test$repr, save = FALSE)
  }

  print(paste0("Adding model: ", model_or_test$repr, ""))

  if (!recompute & model_or_test$repr %in% names(model_computer$models)) {
    print(paste0("Model already exists: ", model_or_test$repr, ""))
    return()
  }

  if (inherits(model_or_test, "model")) {
    model_fits <- apply_to_trials(model_computer$trial_data, function(trial) fit_model(model_or_test, trial))

    model_results <- data.table(t(sapply(model_fits, function(x) x$metrics)))
    model_computer$model_metrics[[model_or_test$repr]] <- model_results

    model_estimates <- lapply(model_fits, function(fit) fit$estimates)
    model_computer$model_estimates[[model_or_test$repr]] <- do.call(rbind, model_estimates)

  } else if (inherits(model_or_test, "test")) {
    test_fits <- apply_to_trials(model_computer$trial_data, function(trial) run_test(model_or_test, trial))
    p_values <- sapply(test_fits, function(x) x$p_value)
    model_computer$model_metrics[[model_or_test$repr]] <- data.table(p_value = p_values)
  } else {
    stop("model_or_test must be a model or test object")
  }

  model_computer$models[[model_or_test$repr]] <- model_or_test

  if (save) {
    save.model_computer(model_computer)
  }
}

#' @title add_models
#' @description Add a list of models to the model computer
#' @param model_computer A model_computer object
#' @param model_list A list of model or test objects
#' @param recompute A boolean specifying whether to recompute the models if its representation already contains results (default = FALSE)
#' @param skip_faulty A boolean specifying whether to skip faulty models (default = TRUE)
#' @return A list of errors if any
add_models <- function(model_computer, model_list, recompute = FALSE, skip_faulty = TRUE) {
  errors <- list()
  for (model in model_list) {
    if (skip_faulty) {
      tryCatch({
        add_model(model_computer, model, recompute = recompute)
      }, error = function(e) {
        # print to log file
        print(model$repr)
        print(e)
        errors[[model$repr]] <<- e
      })
    }else {
      add_model(model_computer, model, recompute = recompute)
    }
  }
  return(errors)
}


drop_model <- function(model_computer, model_repr, save = FALSE) {
  model_exists <- model_repr %in% names(model_computer$model_metrics)
  model_computer$model_metrics[[model_repr]] <- NULL
  model_computer$models[[model_repr]] <- NULL
  model_computer$model_estimates[[model_repr]] <- NULL

  if (save) {
    save.model_computer(model_computer)
  }
  return(model_exists)
}




    #' @title get_value
    #' @description Get a value from the model metrics of the model computer
    #' @param model_computer A model_computer object
    #' @param val The value to get from the model metrics
    #' @return A data.table containing the values for each model where the value is defined
get_value <- function(model_computer, val = "p_value") {
  result <- NULL
  for (model in names(model_computer$models)) {
    if (val %in% names(model_computer$model_metrics[[model]])) {
      if(is.null(unlist(model_computer$model_metrics[[model]][[val]]))){
        warning(paste0("No value for ", val, " in model ", model))
        next
      }

      if (is.null(result)) {
        result <- data.table(model_computer$model_metrics[[model]][[val]])
        setnames(result, model)
      }
      else {
        result[[model]] <- model_computer$model_metrics[[model]][[val]]
      }

    }
  }
  return(result)
}
