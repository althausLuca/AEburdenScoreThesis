N_SIM <- 5000
SEED <- 7
N_PARTICIPANTS <- 100 # per group
k <- 1.5 # susceptibility parameter
TRIAL_TIME <- 180 # max time

GAP_TIME_SHORT <- 750
GAP_TIME_LONG <- 1500

TRIAL_DATA_PATH <- "data/"

# scenario variation settings
# names have to match with SCENARIO_PATTERNS in Scenarios.R
pattern_variation_config <- list(
  "identical_settings" = list(
    scenario_name = "s1",
    folder = "identical_settings/"
  ),
  "shorter_gap_times" = list(
    scenario_name = "s2",
    folder = "shorter_gap_times/",
    shorter_factors = c(1/10, 1/7 , 1/5, 1/3.5, 1/2.5, 1/2,1/1.5, 1/1.25, 1, 1.25, 1.5, 2, 2.5, 3.5,5,7, 10)
  ),
  "longer_event_durations" = list(
    scenario_name = "s3",
    folder = "longer_event_durations/",
    longer_factors = c(10,7, 5, 3.5, 2.5, 1.5, 1.25, 1, 1 / 1.25, 1/1.5, 1 / 2.5, 1 / 3.5, 1/7, 1 / 5, 1 / 10)
  ),
  "more_severe_events" = list(
    scenario_name = "s4",
    folder = "more_severe_events/",
    severity_factors = NULL # to be defined in the scenario
  )
)

trial_size_variation_config <- list(
  folder = "sample_size_variation/",
  sizes = c(100, 50, 30, 20),
  patterns = list(equal = list(factor = NULL),
                  gap_time = list(factor = 0.5),
                  duration = list(factor = 3.5)
  )
)


dir.create(TRIAL_DATA_PATH, recursive = TRUE, showWarnings = FALSE)

# source("R/data_generation/Scenarios.R")
source("R/data_generation/AE_types.R")
source("R/data_generation/trial_data.R")

