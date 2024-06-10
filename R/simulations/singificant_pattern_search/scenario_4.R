library(ggplot2)
library(dplyr)
library(tidyr)

source("R/methods/run_methods.R")

trials_folder <- "data/trials"
model_folder <- "data/model_results"


trial_files <- list.files(trials_folder, full.names = FALSE)
model_fildes <- list.files(model_folder, full.names = FALSE)

model_file <- "Scenario_4_k_1.5.csv"

p_values <- get_values(model_file, "AIC")
sig_p_values <- colMeans(p_values < 0.05)
sig_p_values

source("R/trials/trial_simulation.R")
source("R/Scenarios.R")
source("R/trials/analysis/plots.R")


n_sim <- 5000
n_subjects <- 100 # per group
scenario_name <- "Scenario_4"
shorter <- 0.25
k <- 1.5
scenario <- load_scenario(scenario_name)
scenario$control$severity
scenario$treatment$severity

control.group <- simulate_group(scenario$control, size = n_sim, susceptibility_parameter = list("gamma", k), max_time = 180)
treatment.group <- simulate_group(scenario$treatment, size = n_sim, susceptibility_parameter = list("gamma", k), max_time = 180)

hist_and_box_plot(treatment.group$scores, treatment.group$n_events
  , file_name = paste0("scenario_4_treatment", ".png") , main="Treatment Group"
  ,hist_lim = 100, hist_y_lim = 270)
hist_and_box_plot(control.group$scores, control.group$n_events
  , file_name = paste0("scenario_4_control", ".png")
  , hist_lim = 100 , main="Control Group", hist_y_lim = 270)
