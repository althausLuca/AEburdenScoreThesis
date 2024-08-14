#' This file combines  all the model running functions
#' Models are:
#' - Linear regression
#' - Tweedie regression
#' - Quantile regression
#' - Log-ANOVA

source("R/models/models.R")
source("R/models/fit_models.R")
source("R/models/model_metrics.R")
source("R/models/model_coefficients.R")
source("R/models/model_settings.R")


DEFAULT_MODELS  <- list(
    ANOVA = ANOVA(),
    LOG_ANOVA = LOG_ANOVA(delta=0.001),
    LOG_ANOVA = LOG_ANOVA(delta=1),
    TWEEDIE = TWEEDIE_REGRESSION(),
    QUANTILE_REGRESSION = QUANTILE_REGRESSION(),
    WILCOXON_TEST = WILCOXON_TEST(),
    PERMUTATION_TEST = PERMUTATION_TEST(),
    ZERO_INFLATED_GAMMA = ZERO_INFLATED_GAMMA()
)

#' Define a function to run standard linear regression, Tweedie regression, and Quantile regression
#' Parameters:
#' - score_data: A data frame with two columns scores and group
#' - link.power: link.power for the tweedy regression (default 0)
#' - var.power:  var.power for the tweedy regression (default 1.5)
#' - delta: A small number to add to the scores to avoid log(0) in log anova model
#' Returns a list containing results from different models i.e., lm , tweedie, quantile_regression, log_anova, wilcoxon test
run_models <- function(trial, models= DEFAULT_MODELS ) {
  trial <- check_data(trial)
  results <- list()

  for(model in models){
    model_name <- model$repr
    model_fit <- fit_model(model, trial)
    coefs <- extract_coefficients(model_fit)
    metrics <- extract_metrics(model_fit)

    # merge metrics and coefs
    for(metric_name in names(metrics)){
      coefs[[metric_name]] <- metrics[[metric_name]]
    }

    results[[model_name]] <- coefs
  }

  return(results)
}

#' Define a function to run models from trial data
#' Parameters:
#' - trial_data: A list of data frames with two columns scores and group
#' - trial_pre_procesing: A function trial_df -> trial_df  to preprocess the trial data , e.g subsetting
#' - ... : Additional parameters to pass to run_models
run_models_from_trial_data <- function(trial_data,
                                       trial_pre_procesing = function(x) { return(x) },
                                       ...) {
  print("running models")

  #check if trial_data is a list
  if ("trials" %in% names(trial_data)) {
    trials <- trial_data$trials
  }
  else {
    trials <- trial_data
  }

  results <- vector("list", length = length(trials))

  for (trial_index in seq_along(trials)) {
    trial <- trial_pre_procesing(trials[[trial_index]])
    result <- run_models(trial, ...)
    results[[trial_index]] <- result
  }
  return(results)
}


run_models_to_file <- function(file_name, n_it = -1, link.power = 0, var.power = 1.5, delta = 1) {
  skip_row <- 0

  if (endsWith(file_name, ".csv")) {
    file_name <- substr(file_name, 1, nchar(file_name) - 4)
  }
  output_file_name <- paste0("data/model_results/", file_name, ".csv")
  input_file_name <- paste0("data/trials/", file_name, ".csv")

  # Open CSV file in append mode
  output_file_conn <- file(output_file_name, "w")

  #write a comment in the first row: # test comment
  writeLines("# test comment", output_file_conn)
  close(output_file_conn)

  output_file_conn <- file(output_file_name, "a")

  while (skip_row / 2 != n_it) {
    trial_sample <- tryCatch({
      read.table(input_file_name, header = FALSE, sep = ",", nrow = 2, skip = skip_row)
    }, error = function(e) {
      print(e)
      close(output_file_conn)
      return()
    })

    # if trial_sample is empty, break the loop
    print(length(trial_sample))
    if (length(trial_sample) == 0) {
      print("No more data to read")
      return()
    }

    group1 <- trial_sample[1, 1]
    group2 <- trial_sample[2, 1]
    data1 <- as.numeric(trial_sample[1, -1])
    data2 <- as.numeric(trial_sample[2, -1])
    group_labels <- c(rep(group1, length(data1)), rep(group2, length(data2)))
    df <- data.frame(score = c(data1, data2), group = group_labels)
    model_result <- run_models(df, link.power = link.power, var.power = var.power, delta = delta)
    # store the results
    write.table(data.frame(model_result),
                file = output_file_conn, sep = ",", col.names = skip_row == 0, row.names = FALSE, append = TRUE)
    skip_row <- skip_row + 2

  }
  close(output_file_conn)
  print("Done")
}



