# This file combines  all the model running functions
# Models are:
# - Linear regression
# - Tweedie regression
# - Quantile regression
# - Log-ANOVA

library(readr)

source("R/models/anova/anova_model.R")
source("R/models/tweedie/tweedie_model.R")
source("R/models/quantile_regression/quantile_regression_model.R")
source("R/models/model_settings.R")
source("R/models/permutation_test.R")
source("R/models/wilcoxon_test.R")

#' Define a function to check if the data is valid a dataframe with the columns Score and Group
#' If score and group is used rename them to Score and Group
#' Check that the score are all non negative and the Group is only "control" or "treatment"
#' Parameters:  score_data A data frame with two columns Scores and Group
check_data <- function(score_data) {
  if (!"Score" %in% colnames(score_data)) {
    if ("score" %in% colnames(score_data)) {
      colnames(score_data)[colnames(score_data) == "score"] <- "Score"
    }else {
      stop("The data does not contain a column named 'Score'")
    }
  }
  if (!"Group" %in% colnames(score_data)) {
    if ("group" %in% colnames(score_data)) {
      colnames(score_data)[colnames(score_data) == "group"] <- "Group"
    }else {
      stop("The data does not contain a column named 'Group'")
    }
  }
  if (!all(score_data$Score >= 0)) {
    stop("The data contains negative scores")
  }
  if (!all(score_data$Group %in% c("control", "treatment"))) {
    stop("The data contains invalid group names")
  }
  return(score_data)
}


#' Define a function to run standard linear regression, Tweedie regression, and Quantile regression
#' Parameters:
#' - score_data: A data frame with two columns scores and group
#' - link.power: link.power for the tweedy regression (default 0)
#' - var.power:  var.power for the tweedy regression (default 1.5)
#' - delta: A small number to add to the scores to avoid log(0) in log anova model
#' Returns a list containing results from different models i.e., lm , tweedie, quantile_regression, log_anova, wilcoxon test
run_models <- function(score_data, link.power = 0, var.power = 1.65,
                       delta = 0.001,
                       include_permutation_test = F , n_permutations = 10000) {
  score_data <- check_data(score_data)
  results <- list()

  ## Linear model/ANOVA
  results[[LM]] <- run_anova(score_data)

  ## Log-ANOVA model
  results[[paste0(LOG_ANOVA, "_c_0.001")]] <- run_log_anova(score_data, delta = 0.001)
  results[[paste0(LOG_ANOVA, "_c_1")]] <- run_log_anova(score_data, delta = 1)

  if (include_permutation_test) {
    results[[PERMUTATION_TEST]] <- mean_permutation_test(score_data, n_permutations = n_permutations, return_permutations = F)
  }

  ## Tweedie model
  results[[TWEEDIE]] <- run_tweedie(score_data, var.power = var.power, link.power = link.power)

  ## Quantile regression
  results[[QUANTILE_REGRESSION]] <- run_qauntile_regression(score_data)

  ## Wilcoxon test
  results[[WILCOXON_TEST]] <- run_wilcoxon_test(score_data)
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



