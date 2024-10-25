source("R/data_generation/config_and_init.R")

with(pattern_variation_config$identical_settings, {
  set.seed(SEED)

  scenario <- load_scenario(scenario_name, longer = 1)

  s.c <- scenario$control
  s.t <- scenario$treatment

  stopifnot(identical(s.c$duration, s.t$duration))
  stopifnot(identical(s.c$severity, s.t$severity))
  stopifnot(identical(s.c$gap_time, s.t$gap_time))


  simulate_trials_from_scenario(scenario, susceptibility_parameter = list("gamma", k),
                                n_sim = N_SIM, death = FALSE, save = TRUE,
                                file_name = paste0(scenario_name, "_k_", k)
                                ,result_path = data_result_path)

})