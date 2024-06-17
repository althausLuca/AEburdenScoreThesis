# This file puts together all the model running functions

library(readr)

source("R/models/anova/anova_model.R")
source("R/models/tweedie/tweedie_model.R")
source("R/models/quantile_regression/quantile_regression_model.R")
source("R/models/model_settings.R")
source("R/models/permutation_test.R")

#' Define a function to check if the data is valid a dataframe with the columns Score and Group
#' If score and group is used rename them to Score and Group
#' Check that the score are all non negative and the Group is only "control" or "treatment"
#' Parameters:  score_data A data frame with two columns Scores and Group
check_data <- function(score_data){
    if(!"Score" %in% colnames(score_data)){
        if("score" %in% colnames(score_data)){
        colnames(score_data)[colnames(score_data)=="score"] <- "Score"
        }else{
        stop("The data does not contain a column named 'Score'")
        }
    }
    if(!"Group" %in% colnames(score_data)){
        if("group" %in% colnames(score_data)){
        colnames(score_data)[colnames(score_data)=="group"] <- "Group"
        }else{
        stop("The data does not contain a column named 'Group'")
        }
    }
    if(!all(score_data$Score >= 0)){
        stop("The data contains negative scores")
    }
    if(!all(score_data$Group %in% c("control", "treatment"))){
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
run_models <- function(score_data, link.power = 0, var.power = 1.65, delta = 0.001) {
  score_data <- check_data(score_data)
  results <- list()

  results[[LM]] <- run_anova(score_data)

  ## Log Anova model
  results[[paste0(LOG_ANOVA,"_c_0.001")]] <- run_log_anova(score_data, delta = 0.001)
  results[[paste0(LOG_ANOVA,"_c_1")]] <- run_log_anova(score_data, delta = 1)
  #results[[paste0(LOG_ANOVA,"_c_100000000")]] <- run_anova(score_data, delta =100000000)
  #results[[paste0(LOG_ANOVA,"_c_10000")]] <- run_anova(score_data, delta = 10000)

  ## Tweedie model
  results[[TWEEDIE]] <- run_tweedie(score_data, var.power = var.power, link.power = link.power)

  ## Quantile regression
  results[[QUANTILE_REGRESSION]] <- run_qauntile_regression(score_data)

  return(results)
}






run_methods_from_file <- function(file_name, n_it = -1, link.power = 0, var.power = 1.5, delta = 1) {
  skip_row <- 0

  if(endsWith(file_name, ".csv")) {
    file_name <- substr(file_name, 1, nchar(file_name) - 4)
  }
  output_file_name <- paste0("data/model_results/", file_name, ".csv")
  input_file_name <- paste0("data/trials/", file_name, ".csv")

  # Open CSV file in append mode
  output_file_conn <- file(output_file_name, "w")

  #write a comment in the first row: # test comment
  writeLines("# test comment" , output_file_conn )
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

#' Define a function to get a value from all models
#' Parameters:
#' - table: A data frame with results from different models or the file name containing the results
#' - pattern: A string to match the column names e.g., p_value to get the p-values
#' Returns a data frame containing the values from the models
get_values <- function(table, pattern = "p_value") {
  result <- list()

  if(is.character(table)) {
    if(dirname(table) != ".") {
      file_dir <- dirname(table)
      file_name <- basename(table)
    }
    else {
      file_dir <- "data/model_results/"
      file_name <- table
    }
    table <- read.table(paste0(file_dir, file_name), header = TRUE, sep = ",")
  }

  cols <- grep(pattern, names(table))
  for (c in cols) {
    # Splitting the column name to extract the model name
    dot_splitted_str <- strsplit(names(table)[c], "\\.")[[1]]
    model_name <-dot_splitted_str[1:max(1,length(dot_splitted_str)-1)]
    model_name <- paste(model_name, collapse = ".")

    # Accessing the first row's value as indicated in your example
    result[[model_name]] <- table[[c]]
  }
  data.frame(result)
}

?logLik.rq