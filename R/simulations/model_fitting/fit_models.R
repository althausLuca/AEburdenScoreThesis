source("R/trials/trial_loader.R")
source("R/models/run_models.R")


print("running models")

result_folder <- "data/models/"

trial_data <- load_longer_trials()

trial_data$n_trials

model_result_list <- trial_data$apply_to_each( function(trial) {
  return(run_models(trial, include_permutation_test= TRUE, n_permutations = 10000))
} ,use_parallel = TRUE)

print(model_result_list)

save.image(paste0(result_folder, "longer_trials.RData"))