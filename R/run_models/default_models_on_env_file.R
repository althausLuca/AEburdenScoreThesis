#' Run Models from environment variable contiaing the full file path to the trial data file
#' starting from the directory where R/ and data/ are located

if (FALSE) { # for testing
  source("R/run_models/config.R", local = (run_config <- new.env()))
  file_path <- "data/more_severe_events/all_mostly_severe_experimental.RData"#list.files(run_config$TRIAL_DATA_PATH, recursive = T, full.names = T)[[1]]
  Sys.setenv(TRIAL_FILE = file_path)
  print(Sys.getenv("TRIAL_FILE"))
  trial_data <- load.trial_data(file_path)
  trial_as_df(trial_data$trials[[1]])
  load_model_computer(file_path)
}


file_path <- Sys.getenv("TRIAL_FILE", unset = "")

if (file_path == "") {
  stop("TRIAL_FILE not set")
}

source("R/run_models/run_models.R")
run_models(file_path)