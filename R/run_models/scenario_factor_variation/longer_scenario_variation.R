source("R/trials/trial_loader.R")
source("R/models_and_tests/model_computer.R")
source("../config.R")

trial_folder <- "data/trials/longer_event_durations/"
result_folder <- "results/longer_event_durations/qr/"

MODELS <- QR_MODELS

# list folder
trial_files <- list.files(trial_folder, full.names = TRUE)
FILE_FROM_ENV <- Sys.getenv("TRIAL_FILE" , unset="")

if (FILE_FROM_ENV != "") {
  trial_files <- FILE_FROM_ENV
}

for (trial_file in trial_files) {
  set.seed(7)

  trial_file_name <- basename(trial_file)
  trial_name <- strsplit(trial_file_name, ".csv")[[1]][1]

  print(trial_file)
  trial_data <- get_trial_data(trial_file, result_path = "")
  output_file_name <- trial_file_name

  model_computer <- init_model_computer(trial_data, trial_name, result_folder)
  add_models(model_computer, MODELS)

  print(output_file_name)
}

