library(dplyr)

# clear wd
rm(list = ls())

source("R/Scenarios.R")
source("R/trials/trial_simulation.R")
source("R/trials/trial_analysis.R")

set.seed(7)

n_sim <- 1000
n_subjects <- 100 # per group
scenario_name <- "Scenario_3"


scenario <- load_scenario(scenario_name)
scenario$control[, c(1, 2)]
scenario$treatment[, c(1, 2)]

set.seed(7)

#longer_factors <- c(1.5,1.75)
#longer_factors <- c(2,2.25,2.5,2.75,3,3.25)
longer_factors <- c(3.5,3.75,4,4.25,4.5)
for (l in longer_factors) {
  for (k in c(1.5)) {

    scenario <- load_scenario(scenario_name , longer = l)

    # control group settings
    print(scenario$control[, c(1, 2)])
    scenario$control$severity

    # treatment group settings
    print(scenario$treatment[, c(1, 2)])
    scenario$treatment$severity

    #Sys.sleep(5)

    #scenario$treatment$gap_time <- scenario$control$gap_time*0.50

    # ## Single group scores
    # treatment.constant <- simulate_group(scenario$treatment, susceptibility_parameter = list("constant"))
    # hist_plot(treatment.constant$scores, zero_count = TRUE, mean = TRUE)
    #
    # treatment.gamma.1.5 <- simulate_group(scenario$treatment, susceptibility_parameter = list("gamma", k))
    # hist_plot(treatment.gamma.1.5$scores, zero_count = TRUE, mean = TRUE)
    # hist_plot(treatment.gamma.1.5$susceptibility, mean = TRUE)
    # mean(1 / treatment.gamma.1.5$susceptibility)


    #simulate  trials
    #profvis({
    file_name <- simulate_trials_from_scenario(scenario, susceptibility_parameter = list("gamma", k),
                                               n_sim = n_sim, death = FALSE, save = TRUE, file_name = paste0(scenario_name, "_k_", k, "_l_", l))
    #})

    source("R/methods/run_methods.R")

    # run models
    run_methods_from_file(file_name, n_it = -1)
  }
}
