#' Run Models from environment variable contiaing the full file path to the trial data file
#' starting from the directory where R/ and data/ are located

if (FALSE) { # for testing
  source("R/run_models/config.R", local = (run_config <- new.env()))
  file_path <- list.files(run_config$TRIAL_DATA_PATH, recursive = T, full.names = T)[[1]]
  Sys.setenv(TRIAL_FILE = file_path)
  print(Sys.getenv("TRIAL_FILE"))
  # m <- load_model_computer("results/longer_event_durations/s3_k_1.5_l_0.1.RData")
  # p_values <- get_value(m,"p_value")
  # p_values$tweedie_var_power_1.2_link_power_0
  # p_values$tweedie_var_power_1.2_link_power_0_mle
}

source("R/run_models/run_models.R")

file_path <- Sys.getenv("TRIAL_FILE", unset = "")

if (file_path == "") {
  stop("TRIAL_FILE not set")
}

run_models(file_path)