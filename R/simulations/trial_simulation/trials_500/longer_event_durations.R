source("R/trials/trial_simulation.R")
source("R/simulations/trial_simulation/trials_500/Scenarios_500.R")

folder <- "longer_event_durations_500/"

seed <- 7
n_sim <- 5000
n_subjects <- 100 # per group
scenario_name <- "Scenario_3"
longer_factor <- 3.5
k <- 1.5

longer_factors <- c(10,5,3.5,2.5,1.25,1,1/1.25,1/2.5,1/3.5, 1/5,1/10)

for (longer_factor in longer_factors) {

  set.seed(seed)

  scenario <- load_scenario(scenario_name, longer = longer_factor)

  s.c <- scenario$control
  s.t <- scenario$treatment

  stopifnot(identical(s.c$duration * longer_factor, s.t$duration))
  stopifnot(identical(s.c$severity, s.t$severity))
  stopifnot(identical(s.c$gap_time, s.t$gap_time))

  filename <- paste0(folder , scenario_name, "_k_", k, "_l_", longer_factor)
  simulate_trials_from_scenario(scenario, susceptibility_parameter = list("gamma", k),
                                n_sim = n_sim, death = FALSE, save = TRUE, file_name = filename)
}