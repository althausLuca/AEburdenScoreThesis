source("R/data_generation/config_and_init.R")
#' Initialize the trial_data files for the different scenarios. Without simulating anything
#' simulate <- TRUE will simulate the trial data, takes a long time to run thats why we then simulate each data in parallel

simulate <- FALSE

identical_settings_config <- pattern_variation_config$identical_settings

control_AEs <- DEFAULT_AE_TYPES
experimental_AEs <- DEFAULT_AE_TYPES

file_path <- paste0(TRIAL_DATA_PATH, identical_settings_config$scenario_name, ".RData")

trial_data <- init_trial_data(control_AEs, experimental_AEs, n_trials = N_SIM, max_time = TRIAL_TIME,
                              n_participants = N_PARTICIPANTS, file_path = file_path, susceptibility_parameter = k)

if (simulate) {
  simulate_trial_data(trial_data)
}


## Event Gap Time
gap_time_settings_config <- pattern_variation_config$shorter_gap_times
control_AEs <- DEFAULT_AE_TYPES

factors <- gap_time_settings_config$shorter_factors

for (f in factors) {
  experimental_AEs <- DEFAULT_AE_TYPES
  experimental_AEs <- change_AE_type_property(experimental_AEs, "gap_time", f)

  file_path <- paste0(TRIAL_DATA_PATH, gap_time_settings_config$folder, gap_time_settings_config$scenario_name, "_s_", f, ".RData")

  trial_data <- init_trial_data(control_AEs, experimental_AEs, n_trials = N_SIM, max_time = TRIAL_TIME,
                                n_participants = N_PARTICIPANTS, file_path = file_path, susceptibility_parameter = k)
  if (simulate) {
    simulate_trial_data(trial_data)
  }
}

# Event Duration
duration_settings_config <- pattern_variation_config$longer_event_durations
control_AEs <- DEFAULT_AE_TYPES

factors <- duration_settings_config$longer_factors

for (f in factors) {
  experimental_AEs <- DEFAULT_AE_TYPES
  experimental_AEs <- change_AE_type_property(experimental_AEs, "duration", f)

  file_path <- paste0(TRIAL_DATA_PATH, duration_settings_config$folder, duration_settings_config$scenario_name, "_l_", f, ".RData")

  trial_data <- init_trial_data(control_AEs, experimental_AEs, n_trials = N_SIM, max_time = TRIAL_TIME,
                                n_participants = N_PARTICIPANTS, file_path = file_path, susceptibility_parameter = k)
  if (simulate) {
    simulate_trial_data(trial_data)
  }
}

# Event Severity
pattern_settings <- pattern_variation_config$more_severe_events

default_AEs <- DEFAULT_AE_TYPES

next_level_experimental <- default_AEs
next_level_experimental[[1]]$severity_probabilities <- MOSTLY_MODERATE
next_level_experimental[[2]]$severity_probabilities <- MOSTLY_SEVERE
next_level_experimental[[3]]$severity_probabilities <- MOSTLY_MODERATE

all_mostly_severe_experimental <- default_AEs
all_mostly_severe_experimental[[1]]$severity_probabilities <- MOSTLY_SEVERE
all_mostly_severe_experimental[[2]]$severity_probabilities <- MOSTLY_SEVERE
all_mostly_severe_experimental[[3]]$severity_probabilities <- MOSTLY_SEVERE

all_severe_experimental <- default_AEs
all_severe_experimental[[1]]$severity_probabilities <- c(0, 0, 1)
all_severe_experimental[[2]]$severity_probabilities <- c(0, 0, 1)
all_severe_experimental[[3]]$severity_probabilities <- c(0, 0, 1)


all_mild_control <- default_AEs
all_mild_control[[1]]$severity_probabilities <- c(1, 0, 0)
all_mild_control[[2]]$severity_probabilities <- c(1, 0, 0)
all_mild_control[[3]]$severity_probabilities <- c(1, 0, 0)


severity_patterns <- list(
  default = list("control" = default_AEs, "treatment" = default_AEs),
  next_level_experimental = list("control" = default_AEs, "treatment" = next_level_experimental),
  all_mostly_severe_experimental = list("control" = default_AEs, "treatment" = all_mostly_severe_experimental),
  all_severe_experimental = list("control" = default_AEs, "treatment" = all_severe_experimental),
  all_severe_experimental_all_mild_control = list("control" = all_mild_control, "treatment" = all_severe_experimental)
)

folder <- paste0(TRIAL_DATA_PATH, pattern_settings$folder)
dir.create(folder, recursive = TRUE, showWarnings = FALSE)

for (p in 1:length(severity_patterns)) {
  name <- names(severity_patterns)[p]
  AEs <- severity_patterns[[name]]

  file_path <- paste0(folder, name, ".RData")

  trial_data <- init_trial_data(AEs$control, AEs$treatment, n_trials = 5000, max_time = TRIAL_TIME,
                                n_participants = N_PARTICIPANTS, file_path = file_path, susceptibility_parameter = k)
  if (simulate) {
    simulate_trial_data(trial_data)
  }
}

### TRIAL SIZE VARIATION
control_AEs <- DEFAULT_AE_TYPES
for (pattern in names(trial_size_variation_config$patterns)) {
  f <- trial_size_variation_config$patterns[[pattern]]$factor
  experimental_AEs <- DEFAULT_AE_TYPES
  if (!is.null(f)) {
    experimental_AEs <- change_AE_type_property(experimental_AEs, pattern, f)
  }
  for (size in trial_size_variation_config$sizes) {
    file_path <- paste0(TRIAL_DATA_PATH, trial_size_variation_config$folder, pattern, "_", size, ".RData")
    trial_data <- init_trial_data(control_AEs, experimental_AEs, n_trials = N_SIM, max_time = TRIAL_TIME,
                                  n_participants = size, file_path = file_path, susceptibility_parameter = k)
    if (simulate) {
      simulate_trial_data(trial_data)
    }
  }
}

