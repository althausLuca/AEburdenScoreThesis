source("R/data_generation/config_and_init.R")

with(pattern_variation_config$shorter_gap_times, {
  for (shorter_factor in shorter_factors) {
    set.seed(SEED)

    scenario <- load_scenario(scenario_name, shorter = shorter_factor)

    s.c <- scenario$control
    s.t <- scenario$treatment

    stopifnot(identical(s.c[[1]]$duration, s.t[[1]]$duration))
    stopifnot(identical(s.c[[1]]$severity_probabilities, s.t[[1]]$severity_probabilities))
    stopifnot(identical(s.c[[1]]$gap_time * shorter_factor, s.t[[1]]$gap_time))

    filename <- paste0("shorter_gap_times/", scenario_name, "_k_", k, "_s_", shorter_factor)

    simulate_trials_from_scenario(scenario, susceptibility_parameter = list("gamma", k),
                                  n_sim = N_SIM, death = FALSE, save = TRUE, file_name = filename
                  ,result_path = data_result_path)

  }
})