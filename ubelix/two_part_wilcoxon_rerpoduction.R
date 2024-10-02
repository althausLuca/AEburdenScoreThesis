
source("R/models_and_tests/models_and_tests.R")
source("R/models_and_tests/model_computer.R")
source("R/trials/trial_loader.R")

model_computer <- load_model_computer("results/shorter_gap_times/Scenario_2_k_1.5_s_1.RData")

model_computer$model_metrics$two_part_wilcoxon$p_value
mean(model_computer$model_metrics$two_part_wilcoxon$p_value < 0.05)

trial_data <- model_computer$trial_data
summary(trial_data)

trial <- trial_data$trials[[1]]
two_part_test(trial, test = "wilcoxon")
r <- trial_data$apply_to_each(two_part_test, test = "wilcoxon")

get_T(trial, test = "wilcoxon")