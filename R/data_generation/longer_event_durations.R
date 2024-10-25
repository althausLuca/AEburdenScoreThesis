source("R/data_generation/config_and_init.R")

with(pattern_variation_config$longer_event_durations, {
  for (longer_factor in longer_factors) {
    set.seed(SEED)

    scenario <- load_scenario(scenario_name, longer = longer_factor)

    s.c <- scenario$control
    s.t <- scenario$treatment

    stopifnot(identical(s.c[[1]]$duration * longer_factor, s.t[[1]]$duration))
    stopifnot(identical(s.c[[1]]$severity_probabilities, s.c[[1]]$severity_probabilities))
    stopifnot(identical(s.c[[1]]$gap_time, s.t[[1]]$gap_time))

    filename <- paste0(folder, scenario_name, "_k_", k, "_l_", longer_factor)
    simulate_trials_from_scenario(scenario, susceptibility_parameter = list("gamma", k),
                                  n_sim = N_SIM, death = FALSE, save = TRUE, file_name = filename,result_path = data_result_path)
  }
})