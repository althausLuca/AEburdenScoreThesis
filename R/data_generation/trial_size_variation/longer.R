source("R/data_generation/config_and_init.R")
source("R/trials/trial_loader.R")

for (size in c(50, 30, 20)) {
  set.seed(SEED)

  trial_data <- load_longer_trials()
  file_path <- paste0(data_result_path, "sample_size_variation/longer_",size,".csv")


  #generate new tiral data with smaller group sizes
  trial_data$trials <- lapply(trial_data$trials, function(trial) {
    return(trial_sub_sampler(trial, group_size = size))
  })

  save.trial_data(trial_data, file_path = file_path)

}