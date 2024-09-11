source("R/trials/trial_simulation.R")
source("R/simulations/trial_simulation/trials_500/Scenarios_500.R")

folder <- "shorter_gap_times_500/"

seed <- 7
n_sim <- 5000
n_subjects <- 100 # per group
scenario_name <- "Scenario_2"
shorter_factor <- 0.5
k <- 1.5

shorter_factors <- c(seq(0.1, 1, by = 0.1), 1.25, 2, 2.5, 5, 10)

for (shorter_factor in shorter_factors) {
  set.seed(7)

  scenario <- load_scenario(scenario_name, shorter = shorter_factor)

  s.c <- scenario$control
  s.t <- scenario$treatment

  stopifnot(identical(s.c$duration, s.t$duration))
  stopifnot(identical(s.c$severity, s.t$severity))
  stopifnot(identical(s.c$gap_time * shorter_factor, s.t$gap_time))

  filename <- paste0(folder , scenario_name, "_k_", k, "_s_", shorter_factor)

  simulate_trials_from_scenario(scenario, susceptibility_parameter = list("gamma", k),
                                n_sim = n_sim, death = FALSE, save = TRUE, file_name = filename)

}