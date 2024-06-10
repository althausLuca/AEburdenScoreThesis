library(dplyr)
library(profvis)

# clear wd
rm(list = ls())

source("R/Scenarios.R")
source("R/trials/trial_simulation.R")
source("R/trials/trial_analysis.R")

set.seed(7)

n_sim <- 1000
n_subjects <- 100 # per group
scenario_name <- "Scenario_2"


scenario <- load_scenario(scenario_name)
scenario$control[, c(1, 2)]
scenario$treatment[, c(1, 2)]

shorter_factors <- c(0.5)
#shorter_factors <- seq(0.55,1 ,by=0.05)

for (s in shorter_factors) {
  for (k in c(1.5)) {
    scenario <- load_scenario(scenario_name , shorter = s)

    # control group settings
    print(scenario$control[, c(1, 2)])
    scenario$control$severity

    # treatment group settings
    print(scenario$treatment[, c(1, 2)])
    scenario$treatment$severity

    Sys.sleep(5)

    #scenario$treatment$gap_time <- scenario$control$gap_time*0.50

    ## Single group scores
    treatment.constant <- simulate_group(scenario$treatment, susceptibility_parameter = list("constant"))
    hist_plot(treatment.constant$scores, zero_count = TRUE, mean = TRUE)

    treatment.gamma.1.5 <- simulate_group(scenario$treatment, susceptibility_parameter = list("gamma", k))
    hist_plot(treatment.gamma.1.5$scores, zero_count = TRUE, mean = TRUE)
    hist_plot(treatment.gamma.1.5$susceptibility, mean = TRUE)
    mean(1 / treatment.gamma.1.5$susceptibility)



    file_name <- simulate_trials_from_scenario(scenario, susceptibility_parameter = list("gamma", k),
                                               n_sim = n_sim, death = FALSE, save = TRUE, file_name = paste0(scenario_name, "_k_", k, "_s_", s))

    source("R/methods/run_methods.R")

    # run models
    run_methods_from_file(file_name, n_it = -1)
  }
}
