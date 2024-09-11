source("R/Scenarios.R")
source("R/trials/trial_simulation.R")


seed <- 7
n_sim <- 5000
n_subjects <- 100 # per group
scenario_name <- "Scenario_2"
shorter_factor <- 0.5
k <- 1.5

# shorter_factors <- c(1.25, 2, 2.5, 5, 10)
shorter_factors <- 1/shorter_factors

for (shorter_factor in shorter_factors) {
  set.seed(7)

  scenario <- load_scenario(scenario_name, shorter = shorter_factor)

  s.c <- scenario$control
  s.t <- scenario$treatment

  stopifnot(identical(s.c$duration, s.t$duration))
  stopifnot(identical(s.c$severity, s.t$severity))
  stopifnot(identical(s.c$gap_time * shorter_factor, s.t$gap_time))

  filename <- paste0("shorter_gap_times/" , scenario_name, "_k_", k, "_s_", shorter_factor)

  simulate_trials_from_scenario(scenario, susceptibility_parameter = list("gamma", k),
                                n_sim = n_sim, death = FALSE, save = TRUE, file_name = filename)

}