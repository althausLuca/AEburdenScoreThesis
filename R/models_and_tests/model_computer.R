library(data.table)
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
      print(paste0("Loaded existing results from: ", file_path))
      return(model_computer)
    }
  }

  result <- new.env()
  name <- name
  path <- path
  class(result) <- "model_computer"

  result$model_metrics <- list()
  result$trial_data <- trial_data
  result$models <- list()
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

  if (!recompute & model_or_test$repr %in% names(model_computer$model_metrics)) {
    print(paste0("Model already exists: ", model_or_test$repr, ""))
    return()
  }

  if (inherits(model_or_test, "model")) {
    model_fits <- model_computer$trial_data$apply_to_each(function(trial) fit_model(model_or_test, trial), use_parallel = FALSE)
    model_restults <- data.table(t(sapply(model_fits, function(x) x$estimates)))
    model_restults$p_value <- lapply(model_fits, function(fit) fit$p_value)
    model_restults$AIC <- sapply(model_fits, function(fit) fit$AIC)
  }
  else if (inherits(model_or_test, "test")) {
    test_fits <- model_computer$trial_data$apply_to_each(function(trial) run_test(model_or_test, trial), use_parallel = FALSE)
    p_values <- sapply(test_fits, function(x) x$p_value)
    model_restults <- data.table(p_value = p_values)
  }
  else {
    stop("model_or_test must be a model or test object")
  }
  model_computer$model_metrics[[model_or_test$repr]] <- model_restults

  model_computer$models[[model_or_test$repr]] <- model_or_test
  if (save) {
    save.model_computer(model_computer)
  }
}

add_models <- function(model_computer, model_list, recompute = FALSE, skip_faulty = TRUE) {
  for (model in model_list) {
    tryCatch({
      add_model(model_computer, model, recompute = recompute)
    }, error = function(e) {
      # print to log file
      sink()
      file <- "logs/error_log.txt"
      if (!file.exists(file)) {
        writeLines("Error Log", file)
      }
      writeLines(paste0("Error: ", model, e$message), file)
      if (!skip_faulty) {
        stop(e)
      }
    })
  }
}


drop_model <- function(model_computer, model_repr, save = FALSE) {
  model_exists <- model_repr %in% names(model_computer$model_metrics)
  model_computer$model_metrics[[model_repr]] <- NULL
  model_computer$models[[model_repr]] <- NULL
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


p_value_plot <- function(model_computer, save = NULL, models_to_exclude = NULL) {
  source("R/evaluation/analysis_and_comparison/p_value_plot.R")

  p_values <- get_value(model_computer, "p_value")
  model_names <- names(p_values)

  p_value_plot <- p_value_plot_handler()
  for (name in model_names) {
    if (!is.null(models_to_exclude) & name %in% models_to_exclude) {
      next
    }
    p_value_plot$add(unlist(p_values[[name]]), name)
  }

  if (!is.null(save)) {
    dir.create(dirname(save), recursive = TRUE, showWarnings = FALSE)
    p_value_plot$save(save)
  }

  plot <- p_value_plot$plot()
  return(plot)
}


p_value_cdf_table <- function(model_computer, values = c(0.05, 0.25, 0.5, 0.75)) {
  source("R/models_and_tests/model_settings.R")
  p_values <- get_value(model_computer, "p_value")
  model_names <- names(p_values)

  df <- data.frame(sapply(values, function(val) colMeans(p_values < val)))
  df <- round(df, 3)
  # set colnames
  colnames(df) <- values
  rownames(df) <- NULL
  df <- cbind(model = 0, df)
  df$model <- lapply(model_names, map_labels)
  return(df)
}
