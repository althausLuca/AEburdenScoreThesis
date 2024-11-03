source("R/trials/trial_loader.R")
source("R/models_and_tests/model_computer.R")
source("R/run_models/config.R", local = (config <- new.env()))

run_models <- function(trial_file_path, models = config$DEFAULT_MODELS, recompute = config$CLEAR_ALL) {
  # modify the path to the results folder and put .RData as the file extention
  result_path <- gsub(config$TRIAL_DATA_PATH, config$MODEL_RESULT_PATH, trial_file_path)
  result_path <- gsub("data/", config$MODEL_RESULT_PATH, result_path) # for the case of a special trial folder
  result_path <- paste0(tools::file_path_sans_ext(result_path), ".RData")

  print(trial_file_path)

  print("storing results in:")
  print(result_path)

  stop("stop here")

  result_folder <- dirname(result_path)
  model_computer_name <- basename(result_path)

  trial_data <- get_trial_data(trial_file_path, result_path = "")
  model_computer <- init_model_computer(trial_data, model_computer_name, result_folder, check_exists = !recompute)
  errors <- add_models(model_computer, models , skip_faulty= !config$HALT_ON_ERROR)
  print(errors)
  if (length(errors) > 0) {
    source("R/run_models/logger.R")
    logger <- init_logger(model_computer_name)
    for (error in errors) {
      logger$write(error)
    }
  }
}