library(ggplot2)
library(dplyr)
library(tidyr)

source("R/methods/run_methods.R")

trials_folder <- "../../../data_old/trials"
model_folder <- "data/model_results"


trial_files <- list.files(trials_folder, full.names = FALSE)
model_fildes <- list.files(model_folder, full.names = FALSE)

model_file <- "Scenario_4_k_1.5.csv"

p_values <- get_values(model_file, "AIC")
sig_p_values <- colMeans(p_values < 0.05)
sig_p_values
