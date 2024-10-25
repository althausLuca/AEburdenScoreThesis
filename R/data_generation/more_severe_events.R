source("R/data_generation/config_and_init.R")

with(pattern_variation_config$more_severe_events, {
  scenario <- load_scenario("s1") # default scenario

  default_scenario <- scenario

  scenario$control$severity

  next_level_experimental <- scenario
  next_level_experimental$treatment[[1]]$severity_probabilities <- MOSTLY_MODERATE
  next_level_experimental$treatment[[2]]$severity_probabilities <- MOSTLY_SEVERE
  next_level_experimental$treatment[[3]]$severity_probabilities <- MOSTLY_MODERATE

  all_mostly_severe_experimental <- scenario
  all_mostly_severe_experimental$treatmen[[1]]$severity_probabilities <- MOSTLY_SEVERE
  all_mostly_severe_experimental$treatmen[[2]]$severity_probabilities <- MOSTLY_SEVERE
  all_mostly_severe_experimental$treatmen[[3]]$severity_probabilities <- MOSTLY_SEVERE

  all_severe_experimental <- scenario
  all_severe_experimental$treatment[[1]]$severity_probabilities <- c(0, 0, 1)
  all_severe_experimental$treatment[[2]]$severity_probabilities <- c(0, 0, 1)
  all_severe_experimental$treatment[[3]]$severity_probabilities <- c(0, 0, 1)


  all_severe_experimental_all_mild_control <- scenario
  all_severe_experimental_all_mild_control$treatment[[1]]$severity_probabilities <- c(0, 0, 1)
  all_severe_experimental_all_mild_control$treatment[[2]]$severity_probabilities <- c(0, 0, 1)
  all_severe_experimental_all_mild_control$treatment[[3]]$severity_probabilities <- c(0, 0, 1)

  all_severe_experimental_all_mild_control$control[[1]]$severity <- c(1, 0, 0)
  all_severe_experimental_all_mild_control$control[[2]]$severity <- c(1, 0, 0)
  all_severe_experimental_all_mild_control$control[[3]]$severity <- c(1, 0, 0)


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
    filename <- paste0("more_severe_events/", name)

    simulate_trials_from_scenario(scenario, susceptibility_parameter = list("gamma", k),
                                  n_sim = N_SIM, death = FALSE, save = TRUE, file_name = filename)
  }
})