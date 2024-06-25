source("R/trials/trial_loader.R")
source("R/models/run_models.R")
source("R/models/model_results.R")

trial_folder <- "data/trials/longer_event_durations/"
model_folder <- "data/models/longer_event_durations/"

dir.create(model_folder, recursive = TRUE, showWarnings = FALSE)

# list folder
trial_files <- list.files(trial_folder, full.names = TRUE)


trial_file <- trial_files[1]
for (trial_file in trial_files) {
  trial_file_name <- tail(strsplit(trial_file, "/")[[1]],1)
  output_file_name <- paste0(model_folder, trial_file_name)
  output_file_name <- gsub(".csv", ".RData", output_file_name)

  print(trial_file_name)

  trial_data <- get_trial_data(trial_file, result_path = "") # load the trial data
  print(trial_data$n_trials)

  model_result_list <- trial_data$apply_to_each(function(trial) {
    return(run_models(trial, include_permutation_test = TRUE, n_permutations = 10000))
  }, use_parallel = TRUE)
  print(length(model_result_list))

  print("storing to")
  print(output_file_name)
  model_results <- init_model_results(model_result_list)
  save.image(output_file_name)
}


