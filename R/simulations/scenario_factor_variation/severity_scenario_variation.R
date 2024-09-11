source("R/helpers.R")
source("R/models_and_tests/model_computer.R")

trial_folder <- "data/trials/more_severe_events"
result_folder <- "results/more_severe_events/"

# list folder
trial_files <- list.files(trial_folder, full.names = TRUE)
FILE_FROM_ENV <-"data/trials/more_severe_events/defualt.csv"     #Sys.getenv("TRIAL_FILE" , unset="")

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
  drop_model(model_computer, "two_part_wilcoxon")
  add_models(model_computer, DEFAULT_MODELS)

  print(output_file_name)
}
