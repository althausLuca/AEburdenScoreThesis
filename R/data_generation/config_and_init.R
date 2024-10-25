N_SIM <- 5000
SEED <- 7
N_SUBJECTS <- 100 # per group
k <- 1.5 # susceptibility parameter


GAP_TIME_SHORT <- 750
GAP_TIME_LONG <- 1500


data_result_path <- "data/trials2/"


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
    shorter_factors = c(1/10,1/5,1/2.5,1/2,1/1.25,1,1.25, 2, 2.5, 5, 10)
    ),
    "longer_event_durations" = list(
    scenario_name = "s3",
    folder = "longer_event_durations/",
    longer_factors = c(10,5,3.5,2.5,1.25,1,1/1.25,1/2.5,1/3.5, 1/5,1/10)
    ),
      "more_severe_events" = list(
    scenario_name = "s4",
    folder = "more_severe_events/",
    severity_factors = NULL # to be defined in the scenario
    )
)

dir.create(data_result_path , recursive = TRUE)

source("R/data_generation/Scenarios.R")
source("R/trials/trial_simulation.R")
