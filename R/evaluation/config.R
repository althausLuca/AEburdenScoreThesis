source("R/models_and_tests/models_and_tests.R")
source("R/run_models/config.R", local = (run_config <- new.env()))
source("R/evaluation/model_settings.R")

# default should be the same as in run_models/config.R  (run_config$MODEL_RESULT_PATH)
# MODEL_DATA_PATH <- paste0(run_config$MODEL_RESULT_PATH)
MODEL_DATA_PATH <- "results/"
PLOT_PATH <- paste0("plots/method_evaluation/", gsub("results/", "", MODEL_DATA_PATH))

#scenario_factor_variation default files
DEFAULT_DURATION_VAR_FILE <- paste0(MODEL_DATA_PATH, "longer_event_durations/s3_l_3.5.RData")
DEFAULT_GAP_TIME_VAR_FILE <- paste0(MODEL_DATA_PATH, "shorter_gap_times/s2_s_0.5.RData")
EQUAL_SETTIGS_FILE <- paste0(MODEL_DATA_PATH, "s1.RData")
TRIAL_SIZE_VARIATION_PATH <- paste0(MODEL_DATA_PATH,"sample_size_variation/")


DURATION_VARIATION_PLOT_PATH <- paste0(PLOT_PATH, "duration_variation.pdf")
GAP_TIME_VARIATION_PLOT_PATH <- paste0(PLOT_PATH, "gap_time_variation.pdf")
SEVERITY_INCREASE_PLOT_PATH <- paste0(PLOT_PATH, "severity_increase.pdf")

#check if default files exit
if (!file.exists(DEFAULT_GAP_TIME_VAR_FILE)) {
  print(paste0("File ", DEFAULT_GAP_TIME_VAR_FILE, " does not exist"))
}
if (!file.exists(DEFAULT_DURATION_VAR_FILE)) {
  print(paste0("File ", DEFAULT_DURATION_VAR_FILE, " does not exist"))
}
if (!file.exists(EQUAL_SETTIGS_FILE)) {
  print(paste0("File ", EQUAL_SETTIGS_FILE, " does not exist"))
}

dir.create(PLOT_PATH, recursive = TRUE , showWarnings = FALSE)


#' Loading function for quick acces to selected trial data objects
load_shorter_trials <- function() load_model_computer(DEFAULT_GAP_TIME_VAR_FILE)$trial_data
load_longer_trials <- function() load_model_computer(DEFAULT_DURATION_VAR_FILE)$trial_data
load_equal_trials <- function() load_model_computer(EQUAL_SETTIGS_FILE)$trial_data
