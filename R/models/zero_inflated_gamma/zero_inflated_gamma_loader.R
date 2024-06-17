ls()

load("inflated_gamma_800.RData")
source("zero_inflated_gamma_functions.R")

ls()
library(gamlss)


p_values <- c()

# Loop through each trial
for(trial_index in seq_along(trial_indices)) {
  real_trial_index <- trial_indices[[trial_index]]
  trial <- trial_data$trials[[real_trial_index]]

  # Compute observed difference for the current trial
  trial_mean_estimates <- get_mean_estimate(trial)

  observed_difference <- trial_mean_estimates[2] - trial_mean_estimates[1]

  # Use precomputed differences for permutation test
  perm_differences <- unlist(lapply(trial_mu_estimates[[trial_index]], function(element) element[[2]] - element[[1]]))

  # Calculate p-value
  p_value <- mean(abs(perm_differences) >= abs(observed_difference))
  p_values <- c(p_values, p_value)

  # Print results for each trial
  cat("Trial:", trial_index, "\n")
  cat("Observed Difference:", observed_difference, "\n")
  cat("P-value:", p_value, "\n")
}

# Print all p-values
print(p_values)
