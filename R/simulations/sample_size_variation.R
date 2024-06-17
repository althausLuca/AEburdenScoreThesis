source("R/trials/trial_loader.R")

size <- 20

model_file <- "Scenario_2_k_1.5_s_0.5.csv"
trial_data <- load_trial_data(model_file)


#supset trials
sub_set_trials <- trial_data$apply_to_each(trial_sub_sampler , group_size = size)

result_path <- paste0("data/workspaces/smaller_samples_s_",size,".RData")

source("R/models/run_models.R")
# run all models including permuation test
for(trial in sub_set_trials){
  results <- run_models(trial)
  results$parmutation_p_value <- mean_permutation_test(trial,return_permutations = FALSE)
  save.image(result_path)
}





