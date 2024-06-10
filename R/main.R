library(dplyr)
library(profvis)

# clear wd
rm(list = ls())

source("R/Scenarios.R")
source("R/trials/trial_simulation.R")
source("R/trials/trial_analysis.R")

set.seed(7)

n_sim <- 10000
n_subjects <- 100 # per group
scenario_name <- "Scenario_2"


scenario <- load_scenario(scenario_name)
scenario$control[, c(1, 2)]
scenario$treatment[, c(1, 2)]

for (k in c(0.01, 0.1, 0.5, 1, 5, 10, 1000000000000000)) {
  scenario <- load_scenario(scenario_name)

  # control group settings
  scenario$control[, c(1, 2)]
  scenario$control$severity

  # treatment group settings
  scenario$treatment[, c(1, 2)]
  scenario$treatment$severity

  #scenario$treatment$gap_time <- scenario$control$gap_time*0.50

  ## Single group scores
  treatment.constant <- simulate_group(scenario$treatment, susceptibility_parameter = list("constant"))
  hist_plot(treatment.constant$scores, zero_count = TRUE, mean = TRUE)

  treatment.gamma.1.5 <- simulate_group(scenario$treatment, susceptibility_parameter = list("gamma", k))
  hist_plot(treatment.gamma.1.5$scores, zero_count = TRUE, mean = TRUE)
  hist_plot(treatment.gamma.1.5$susceptibility, mean = TRUE)
  mean(1 / treatment.gamma.1.5$susceptibility)


  #simulate  trials
  #profvis({
  file_name <- simulate_trials_from_scenario(scenario, susceptibility_parameter = list("gamma", 1),
                                             n_sim = n_sim, death = FALSE, save = TRUE, file_name = paste0(scenario_name, "_k_", k, "_s_", SHORTER, "_l_", LONGER))
  #})

  source("R/methods/run_methods.R")

  # run models
  run_methods_from_file(file_name, n_it = -1)
}