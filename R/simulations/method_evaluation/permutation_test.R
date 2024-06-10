source("R/trials/trial_loader.R")
source("R/methods/permutation_test.R")

trial_data <- load_equal_trials()


mean_permutation_test(trial_data$trials[[10]])


permutations_tests <- trial_data$apply_to_each(function (trial) mean_permutation_test(trial = trial , return_permutations=F )$p_value,
                                               as.df = TRUE,limit=500)

permutations_tests


perm_test_10 <- mean_permutation_test(trial_data$trials[[10]],n_sim = 1000000)
perm_test_10
perm_diff <- abs(perm_test_10$permuted_means$treatment - perm_test_10$permuted_means$control)
observed_diff <-  abs(perm_test_10$observed_means$control - perm_test_10$observed_means$treatment)
hist(perm_diff , xlim=c(0,max(observed_diff,(perm_diff))+1), breaks = 100, main = "Permutation Test", xlab = "Difference in Means")
abline(v =observed_diff, col = "red" )



