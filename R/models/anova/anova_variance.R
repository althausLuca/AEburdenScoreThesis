source("R/Scenarios.R")
source("R/methods/run_methods.R")

library(ggplot2)
library(car)

scenario <- SCENARIOS$severity
data <- load_data(scenario)
scores <- data$scores

var_controls <- numeric()
var_treatments <- numeric()
var_log_controls <- numeric()
var_log_treatments <- numeric()


welch_test_results <- data.frame(test_statistic = numeric(), p_value = numeric())
levene_results <- data.frame(test_statistic = numeric(), p_value = numeric())  #https://datatab.de/tutorial/levene-test

for (score_df in scores) {
  control_scores <- score_df$scores[score_df$groups == "control"]
  treatment_scores <- score_df$scores[score_df$groups == "treatment"]

  var_controls <- c(var_controls, var(control_scores))
  var_treatments <- c(var_treatments, var(treatment_scores))
  var_log_controls <- c(var_log_controls, var(log(control_scores)))
  var_log_treatments <- c(var_log_treatments, var(log(treatment_scores)))

  combined_scores <- c(control_scores, treatment_scores)
  group_factor <- factor(c(rep("control", length(control_scores)), rep("treatment", length(treatment_scores))))

  test_result <- leveneTest(log(combined_scores), group_factor)

  # Extracting the test statistic and p-value
  levene_results <- rbind(levene_results, data.frame(test_statistic =  test_result$`F value`[1],
                                                     p_value = test_result$`Pr(>F)`[1]))

  # Welch t-test
  welch_test_result <- t.test(log(control_scores), log(treatment_scores), var.equal = FALSE)
  welch_test_results <- rbind(welch_test_results, data.frame(test_statistic = welch_test_result$statistic,
                                                             p_value = welch_test_result$p.value))
}

levene_results$p_value <0.05
var_treatments > var_controls
welch_test_results$p_value < 0.05
