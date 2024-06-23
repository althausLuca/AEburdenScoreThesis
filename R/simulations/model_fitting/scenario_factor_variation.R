source("R/trials/trial_loader.R")
source("R/models/run_models.R")

trial_folder <- "data/trials/shorter_gap_times"
model_folder <- "data/models/shorter_gap_times"
dir.create(model_folder, recursive = TRUE, showWarnings = FALSE)

# list folder
trial_files <- list.files(trial_folder, full.names = TRUE)


trial_file <- trial_files[1]
for (trial_file in trial_files) {
  trial_file_name <- strsplit(trial_file, "/")[[1]][4]
  output_file_name <- paste0(model_folder, "/", trial_file_name)
  output_file_name <- gsub(".csv", ".RData", outp
                           ut_file_name)

  print(trial_file_name)

  trial_data <- get_trial_data(trial_file, result_path = "") # load the trial data
  print(trial_data$n_trials)

  model_result_list <- trial_data$apply_to_each(function(trial) {
    return(run_models(trial, include_permutation_test = TRUE, n_permutations = 10000))
  }, use_parallel = TRUE)
  print(length(model_result_list))
  save.image(output_file_name)
}

# write.table(data.frame(model_result_list),
#             file = output_file_name, sep = ",", col.names = skip_row == 0, row.names = FALSE)
#


