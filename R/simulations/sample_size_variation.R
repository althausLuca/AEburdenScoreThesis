source("R/trials/trial_loader.R")
source("R/models/run_models.R")

size <- 30

result_path <- paste0("data/workspaces/equal_samples_", size, ".RData")
trial_data <- load_equal_trials()

result <- run_models_from_trial_data(trial_data,
                                     function(trial) {
                                       return(trial_sub_sampler(trial, group_size = size))
                                     },
                                     include_permutation = TRUE ,
                                     n_permutations = 10000
)

save.image(result_path)
