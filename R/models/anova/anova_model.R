#' Define a function to run log anova model
#' Parameters:
#' - score_data: A data frame with two columns score and group
#' - c: A small number to add to the scores to avoid log(0) in log anova model
#' - delta: alias for c
#' Returns a list containing results from log anova model
run_log_anova <- function(score_data, c = 1 , delta = c) {

  score_data$Score <- log(score_data$Score + delta)

  # Fit the linear model
  lm_result <- lm(Score ~ Group, data = score_data)

  # Summary of the linear model
  lm_summary <- summary(lm_result)
  print(lm_summary)
  print(anova(lm_result))
  # Extract p-value from the ANOVA table derived from the linear model
  p_value <- anova(lm_result)$`Pr(>F)`[1]

  # Extracting estimates and standard errors
  estimate <- lm_summary$coefficients[2, "Estimate"]
  std_err <- lm_summary$coefficients[2, "Std. Error"]

  # Calculating AIC
  AIC <- AIC(lm_result)
  results <- list("model" = LOG_ANOVA,
                  "p_value" = p_value,
                  "estimate" = estimate,
                  "std_err" = std_err,
                  "AIC" = AIC)

  return(results)
}


#' Define a function to run log anova model
#' Parameters:
#' - score_data: A data frame with two columns scores and groups
#' Returns a list containing results from a linear model
run_anova <- function(score_data) {
  lm_model <- lm(Score ~ Group, data = score_data)
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