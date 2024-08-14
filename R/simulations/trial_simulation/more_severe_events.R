source("R/Scenarios.R")
source("R/trials/trial_simulation.R")

seed <- 7
n_sim <- 5000
n_subjects <- 100 # per group
scenario_name <- "Scenario_4"
k <- 1.5


scenario <- load_scenario(scenario_name)


# reference scenario
equal_scenario <- scenario
equal_scenario$treatment$severity <-equal_scenario$control$severity

filename <- paste0("more_severe_events/",  "equal")
simulate_trials_from_scenario(equal_scenario, susceptibility_parameter = list("gamma", k),
                              n_sim = n_sim, death = FALSE, save = TRUE, file_name = filename)



# next level scenario
next_level_scenario <- scenario

next_level_scenario$treatment$severity  <- list(MOSTLY_MODERATE, MOSTLY_SEVERE, MOSTLY_MODERATE)

filename <- paste0("more_severe_events/",  "next")
simulate_trials_from_scenario(next_level_scenario, susceptibility_parameter = list("gamma", k),
                              n_sim = n_sim, death = FALSE, save = TRUE, file_name = filename)


# severe only scenario
severe_only_scenario <- scenario
severe_only_scenario$treatment$severity  <- list(MOSTLY_SEVERE, MOSTLY_SEVERE, MOSTLY_SEVERE)

filename <- paste0("more_severe_events/",  "severe")
simulate_trials_from_scenario(severe_only_scenario, susceptibility_parameter = list("gamma", k),
                              n_sim = n_sim, death = FALSE, save = TRUE, file_name = filename)

