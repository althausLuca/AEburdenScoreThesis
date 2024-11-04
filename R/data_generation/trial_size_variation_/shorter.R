source("R/data_generation/config_and_init.R")
source("R/trials/trial_loader.R")

dir.create("results/sample_size_variation", recursive = TRUE)

for (size in c(50, 30, 20)) {
  set.seed(SEED)

  trial_data <- load_shorter_trials()

  file_path <- paste0(TRIAL_DATA_PATH, "sample_size_variation/shorter_", size, ".csv")

  #generate new tiral data with smaller group sizes
  trial_data$trials <- lapply(trial_data$trials, function(trial) {
    return(list(control=trial$control, treatment=trial$treatment))
  })

  save.trial_data(trial_data, file_path = file_path)
}

