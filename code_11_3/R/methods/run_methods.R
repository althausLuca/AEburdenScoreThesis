# This file contains functions to run different models and get common results in a common format as
# well as a function to run the models from a file and store the results in a csv file in append mode

library(quantreg)
library(statmod)
library(tweedie)
source("../../../R/methods/methods_settings.R")

#' Define a function to run standard linear regression, Tweedie regression, and Quantile regression
#' Parameters:
#' - score_data: A data frame with two columns scores and group
#' - link.power: link.power for the tweedy regression (default 0)
#' - var.power:  var.power for the tweedy regression (default 1.5)
#' - delta: A small number to add to the scores to avoid log(0) in log anova model
#' Returns a list containing results from different models i.e., lm , tweedie, quantile_regression, log_anova, wilcoxon test
run_models <- function(score_data, link.power = 0, var.power = 1.5, delta = 1) {
  results <- list()

  results[[LM]] <- run_lm(score_data)
  ## Log Anova model
  results[[LOG_ANOVA]] <- run_anova(score_data, delta = delta)

  ## Tweedie model
  results[[TWEEDIE]] <- run_tweedie(score_data, var.power = var.power, link.power = link.power)

  ## Quantile regression
  results[[QUANTILE_REGRESSION]] <- run_qauntile_regression(score_data)

  ## Rank test
  wilcoxon_result <- wilcox.test(score ~ group, data = score_data)
  p_value <- wilcoxon_result$p.value
  results$wilcoxon <- list("model" = WILCOXON,
                           "p_value" = p_value)

  return(results)
}

#' Define a function to run log anova model
#' Parameters:
#' - score_data: A data frame with two columns scores and groups
#' Returns a list containing results from a linear model
run_lm <- function(score_data) {
  lm_model <- lm(score ~ group, data = score_data)
  summary <- summary(lm_model)
  p_value <- summary$coefficients[2, 4]
  std_err <- summary$coefficients[2, 2]
  t_value <- summary$coefficients[2, 3]
  estimate <- summary$coefficients[2, 1]

  AIC <- AIC(lm_model)
  results <- list("model" = LM,
                  "AIC" = AIC,
                  "p_value" = p_value,
                  "std_err" = std_err,
                  "t_value" = t_value,
                  "estimate" = estimate)
  return(results)
}

#' Define a function to run log anova model
#' Parameters:
#' - score_data: A data frame with two columns score and group
#' - delta: A small number to add to the scores to avoid log(0) in log anova model
#' Returns a list containing results from log anova model
run_anova <- function(score_data, delta = 1) {
  ## Log Anova model
  score_data.ln <- score_data
  score_data.ln$score <- log(score_data.ln$score + delta)
  lm_result.ln <- lm(score ~ group, data = score_data.ln)
  anova_result.ln <- aov(score ~ group, data = score_data.ln)
  anova_summary <- summary(anova_result.ln)
  lm_summary <- summary(lm_result.ln)

  p_value <- anova_summary[[1]]$`Pr(>F)`[1]

  # Extracting estimates and standard errors from the linear model summary
  estimate <- lm_summary$coefficients[2, "Estimate"]
  std_err <- lm_summary$coefficients[2, "Std. Error"]

  results <- list("model" = LOG_ANOVA,
                  "p_value" = p_value,
                  "estimate" = estimate,
                  "std_err" = std_err)

  return(results)
}

#' Define a function to run Tweedie regression
#' Parameters:
#' - score_data: A data frame with two columns score and group
#' - link.power: link.power for the tweedy regression (default 0)
#' - var.power:  var.power for the tweedy regression (default 1.5)
#' Returns a list containing results from Tweedie regression
run_tweedie <- function(score_data, var.power = 1.5, link.power = 0) {
  tweedie_model <- glm(score_data$score ~ score_data$group, family =
    tweedie(var.power = var.power, link.power = link.power), control = glm.control(maxit = 100))

  AIC <- AICtweedie(tweedie_model, dispersion = 1)
  # get the p-value for the treatment group coefficient and the standard error and t-value
  summary_tweedy <- summary(tweedie_model)
  p_value <- summary_tweedy$coefficients[2, 4]
  std_err <- summary_tweedy$coefficients[2, 2]
  t_value <- summary_tweedy$coefficients[2, 3]
  estimate <- summary_tweedy$coefficients[2, 1]

  results <- list("model" = TWEEDIE,
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
  quantile_regression_model <- rq(score_data$score ~ score_data$group)
  AIC <- AIC(quantile_regression_model)[1]
  # get the p-value for the treatment group coefficient and the standard error and t-value

  coefficient <- summary(quantile_regression_model)$coefficients[2, 1]
  lower_bound <- summary(quantile_regression_model)$coefficients[2, 2]
  upper_bound <- summary(quantile_regression_model)$coefficients[2, 3]

  sid_quantille_summary <- summary(quantile_regression_model, se = "nid") #or ker
  p_value <- sid_quantille_summary$coefficients[2, "Pr(>|t|)"]
  quantile_std_err <- sid_quantille_summary$coefficients[2, "Std. Error"]
  results <- list("model" = QUANTILE_REGRESSION,
                  "AIC" = AIC,
                  "p_value" = p_value,
                  "estimate" = coefficient,
                  "lower_bound" = lower_bound,
                  "upper_bound" = upper_bound,
                  "std_err" = quantile_std_err)

  return(results)
}


run_methods_from_file <- function(file_name, n_it = -1, link.power = 0, var.power = 1.5, delta = 1) {
  skip_row <- 0

  output_file_name <- paste0("data/model_results/", file_name, ".csv")
  input_file_name <- paste0("data/trials/", file_name, ".csv")

  # Open CSV file in append mode
  file.remove(output_file_name, force = TRUE)

  # Open CSV file in append mode
  output_file_conn <- file(output_file_name, "a")

  while (skip_row / 2 != n_it) {
    trial_sample <- tryCatch({
      read.table(input_file_name, header = FALSE, sep = ",", nrow = 2, skip = skip_row)
    }, error = function(e) {
      print(e)
      close(output_file_conn)
      return()
    })

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
#' - table: A data frame with results from different models
#' - pattern: A string to match the column names
#' Returns a data frame containing the values from the models
get_values <- function(table, pattern = "p_value") {
  result <- list()

  cols <- grep(pattern, names(table))
  for (c in cols) {
    # Splitting the column name to extract the model name
    model_name <- strsplit(names(table)[c], "\\.")[[1]][1]

    # Accessing the first row's value as indicated in your example
    result[[model_name]] <- table[[c]]
  }
  data.frame(result)
}