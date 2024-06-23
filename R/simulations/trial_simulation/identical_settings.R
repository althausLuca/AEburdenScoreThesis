source("R/Scenarios.R")
source("R/trials/trial_simulation.R")

seed <- 7
n_sim <- 5000
n_subjects <- 100 # per group
scenario_name <- "Scenario_1"
k <- 1.5

set.seed(7)

scenario <- load_scenario(scenario_name, longer = longer_factor)

s.c <- scenario$control
s.t <- scenario$treatment

stopifnot(identical(s.c$duration, s.t$duration))
stopifnot(identical(s.c$severity, s.t$severity))
stopifnot(identical(s.c$gap_time, s.t$gap_time))


simulate_trials_from_scenario(scenario, susceptibility_parameter = list("gamma", k),
                              n_sim = n_sim, death = FALSE, save = TRUE, file_name = paste0(scenario_name, "_k_", k))

