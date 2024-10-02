source("R/trials/trial_loader.R")
source("R/models_and_tests/model_computer.R")
source("R/simulations/default_models.R")

dir.create("results/sample_size_variation", recursive = TRUE)


for (size in c(50, 30, 20)) {
  set.seed(7)

  trial_data <- load_shorter_trials()

  file_path <- paste0("data/trials/sample_size_variation/shorter_", size, ".csv")

  all <- trial_data$all_data()

  #generate new tiral data with smaller group sizes
  trial_data$trials <- lapply(trial_data$trials, function(trial) {
    return(trial_sub_sampler(trial, group_size = size))
  })

  save.trial_data(trial_data, file_path = file_path)
}

