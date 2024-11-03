source("R/data_generation/config_and_init.R")

with(pattern_variation_config$more_severe_events, {
  scenario <- load_scenario("s1") # default scenario

  default_scenario <- scenario

  next_level_experimental <- scenario
  next_level_experimental$treatment[[1]]$severity_probabilities <- MOSTLY_MODERATE
  next_level_experimental$treatment[[2]]$severity_probabilities <- MOSTLY_SEVERE
  next_level_experimental$treatment[[3]]$severity_probabilities <- MOSTLY_MODERATE

  all_mostly_severe_experimental <- scenario
  all_mostly_severe_experimental$treatment[[1]]$severity_probabilities <- MOSTLY_SEVERE
  all_mostly_severe_experimental$treatment[[2]]$severity_probabilities <- MOSTLY_SEVERE
  all_mostly_severe_experimental$treatment[[3]]$severity_probabilities <- MOSTLY_SEVERE

  all_severe_experimental <- scenario
  all_severe_experimental$treatment[[1]]$severity_probabilities <- c(0, 0, 1)
  all_severe_experimental$treatment[[2]]$severity_probabilities <- c(0, 0, 1)
  all_severe_experimental$treatment[[3]]$severity_probabilities <- c(0, 0, 1)


  all_severe_experimental_all_mild_control <- scenario
  all_severe_experimental_all_mild_control$treatment[[1]]$severity_probabilities <- c(0, 0, 1)
  all_severe_experimental_all_mild_control$treatment[[2]]$severity_probabilities <- c(0, 0, 1)
  all_severe_experimental_all_mild_control$treatment[[3]]$severity_probabilities <- c(0, 0, 1)

  all_severe_experimental_all_mild_control$control[[1]]$severity_probabilities <- c(1, 0, 0)
  all_severe_experimental_all_mild_control$control[[2]]$severity_probabilities <- c(1, 0, 0)
  all_severe_experimental_all_mild_control$control[[3]]$severity_probabilities <- c(1, 0, 0)


  scenarios <- list(default = default_scenario
    , next_level_experimental = next_level_experimental
    , all_mostly_severe_experimental = all_mostly_severe_experimental
    , all_severe_experimental = all_severe_experimental
    , all_severe_experimental_all_mild_control = all_severe_experimental_all_mild_control
  )

  for (s in 1:length(scenarios)) {
    # print variable names
    name <- names(scenarios)[s]
    scenario <- scenarios[[name]]
    filename <- paste0(folder, name)

    simulate_trials_from_scenario(scenario, susceptibility_parameter = list("gamma", k),
                                  n_sim = N_SIM, death = FALSE, save = TRUE,
                                  file_name = filename , result_path = TRIAL_DATA_PATH)
  }
})

folder <- "results/more_severe_events"
source("R/models_and_tests/models_and_tests.R")
files <- list.files(folder, full.names = TRUE)
for(file in files) {
  model_computer <- load_model_computer(file)
  # values <- get_value(model_computer, "p_value")
  # print(colMeans(values < 0.05, na.rm = TRUE))
  print(file)
  print(summary.trial_data(model_computer$trial_data))
}