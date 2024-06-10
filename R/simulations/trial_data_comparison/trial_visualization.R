#Visualize multiple simulations of a specific trial

rm(list = ls())

file_name <- "Scenario_2_k_1.5_s_0.2.csv"

source("R/trials/trial_simulation.R")
source("R/trials/analysis/plots.R")

trial_data <- load_trial_data(file_name)$all_data()

hist_and_box_plot(trial_data$Score[trial_data$Group=="control"], file_name= "control.png")