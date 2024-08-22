source("R/trials/trial_loader.R")
source("../stat_tests/permutation_test.R")

print("Loading shorter trials")
trial_data <- load_shorter_trials()

print("Running mean_permutation_test")
results <- trial_data$apply_to_each(
  function(trial) {
    print("Running mean_permutation_test")
    mean_permutation_test(trial, n_permutations = 10000)
  },
 use_parallel = TRUE , limit = 1000
)

results_df <- do.call(rbind, lapply(results, function(result)c(result$p_value_abs ,result$p_value, result$p_value_l , result$p_value_g)))
results_df <- as.data.frame(results_df)
names(results_df) <- c("p_value_abs", "p_value", "p_value_l", "p_value_g")

abs_proportion <- sum(results_df$p_value_abs < 0.05)
proportio <- sum(results_df$p_value < 0.05)

save.image("data/workspaces/shorter_event_durations_permutation_test.RData")


load("data/workspaces/shorter_event_durations_permutation_test.RData")
results_df <- data$results_df

mean(results_df$p_value_abs < 0.05)
mean(results_df$p_value < 0.05)