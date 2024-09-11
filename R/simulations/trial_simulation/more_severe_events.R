source("R/Scenarios.R")
source("R/trials/trial_simulation.R")

seed <- 7
n_sim <- 5000
n_subjects <- 100 # per group
scenario_name <- "Scenario_1"
k <- 1.5


scenario <- load_scenario(scenario_name)
default_scenario <- scenario

scenario$control$severity

next_level_experimental <- scenario
next_level_experimental$treatment$severity <- list(MOSTLY_MODERATE, MOSTLY_SEVERE, MOSTLY_MODERATE)

all_mostly_severe_experimental <- scenario
all_mostly_severe_experimental$treatment$severity <- list(MOSTLY_SEVERE, MOSTLY_SEVERE, MOSTLY_SEVERE)

all_severe_experimental <- scenario
all_severe_experimental$treatment$severity <- list(c(0, 0, 1), c(0, 0, 1), c(0, 0, 1))


all_severe_experimental_all_mild_control <- scenario
all_severe_experimental_all_mild_control$treatment$severity <- list(c(0, 0, 1), c(0, 0, 1), c(0, 0, 1))
all_severe_experimental_all_mild_control$control$severity <- list(c(1, 0, 0), c(1, 0, 0), c(1, 0, 0))

scenarios <- list( #default = default_scenario
                  #,next_level_experimental = next_level_experimental
                  #, all_mostly_severe_experimental = all_mostly_severe_experimental
                  #, all_severe_experimental = all_severe_experimental
                  all_severe_experimental_all_mild_control = all_severe_experimental_all_mild_control
                  )

for (s in 1:length(scenarios)) {
  # print variable names
  name <- names(scenarios)[s]
  scenario <- scenarios[[name]]
  filename <- paste0("more_severe_events/", name)

  simulate_trials_from_scenario(scenario, susceptibility_parameter = list("gamma", k),
                                n_sim = n_sim, death = FALSE, save = TRUE, file_name = filename)
}