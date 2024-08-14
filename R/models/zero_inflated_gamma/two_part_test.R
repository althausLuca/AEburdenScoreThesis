get_B <- function(trial) {
  treatment_scores <- trial$Score[trial$Group == "treatment"]
  control_scores <- trial$Score[trial$Group == "control"]

  n_treatment <- length(treatment_scores)
  n_control <- length(control_scores)

  n_treatment_0 <- sum(treatment_scores == 0)
  n_control_0 <- sum(control_scores == 0)

  p_hat_control <- (n_control_0 - 0.5) / n_control
  p_hat_treatment <- (n_treatment_0 + 0.5) / n_treatment

  p_hat <- (n_control_0 + n_treatment_0) / (n_control + n_treatment)

  p_mult_term <- p_hat * (1 - p_hat)
  B <- (p_hat_control - p_hat_treatment) / (p_mult_term / n_treatment + p_mult_term / n_control)^0.5

  if (is.na(B)) {
    B <- 0
  }
  return(B)
}

get_T <- function(trial, test = "ttest") {
  treatment_scores <- trial$Score[trial$Group == "treatment"]
  treatment_scores.c <- treatment_scores[treatment_scores > 0]
  control_scores <- trial$Score[trial$Group == "control"]
  control_scores.c <- control_scores[control_scores > 0]

  n_treatment_c <- length(treatment_scores.c)
  n_control_c <- length(control_scores.c)


  if (test == "ttest") {
    T <- t.test(control_scores.c, treatment_scores.c, alternative = "two.sided")$statistic
  } else if (test == "ks") {
    T <- ks.test(control_scores.c, treatment_scores.c, alternative = "two.sided")$statistic
  } else if (test == "wilcoxon") {
    W <- wilcox.test(control_scores.c, treatment_scores.c, alternative = "two.sided")$statistic
    U <- W - (n_control_c * (n_control_c + 1)) / 2 #https://stats.stackexchange.com/questions/65844/wilcoxon-rank-sum-test-in-r
    T <- abs(U - 0.5) / sqrt((n_control_c *
      n_treatment_c *
      (n_control_c + n_treatment_c + 1)) / 12)
  }

  return(T)
}

two_part_statistics <- function(trial, test = "ttest") {
  B <- get_B(trial)
  T <- get_T(trial, test = test)

  squared_then_sum <- B^2 + T^2

  p_value <- 1 - pchisq(squared_then_sum, df = 2)
  return(list(B = B, T = T, p_value = p_value))
}


source("R/trials/trial_loader.R")

trial_data <- load_equal_trials()
T_test_statistics <- "wilcoxon"

trial_1 <- trial_data$trials[[1]]
get_T(trial_1, test = T_test_statistics)

test_statistics <- trial_data$apply_to_each(function(trial) two_part_statistics(trial, test = T_test_statistics), as.df = TRUE)

sig_p_value_rate_equal <- sum(test_statistics$p_value < 0.05) / length(test_statistics$p_value)

trial_data <- load_longer_trials()
test_statistics <- trial_data$apply_to_each(function(trial) two_part_statistics(trial, test = T_test_statistics), as.df = TRUE)

sig_p_value_rate_longer <- sum(test_statistics$p_value < 0.05) / length(test_statistics$p_value)

trial_data <- load_shorter_trials()
test_statistics <- trial_data$apply_to_each(function(trial) two_part_statistics(trial, test = T_test_statistics), as.df = TRUE)

sig_p_value_rate_shorter <- sum(test_statistics$p_value < 0.05) / length(test_statistics$p_value)

# print results
print(paste0("statistics used for T: ", T_test_statistics))
print(paste0("Type 1 Error: ", sig_p_value_rate_equal))
print(paste0("Power (Longer event durations): ", sig_p_value_rate_longer))
print(paste0("Power (Shorter event gap times): ", sig_p_value_rate_shorter))

