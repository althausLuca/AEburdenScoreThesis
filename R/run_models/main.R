source("R/run_models/config.R", local = (config <- new.env()))
source("R/run_models/run_models.R")

trial_data_path <- config$TRIAL_DATA_PATH
trial_files <- list.files(trial_data_path, recursive = T, full.names = T)

for(file in trial_files){
  run_models(file)
}