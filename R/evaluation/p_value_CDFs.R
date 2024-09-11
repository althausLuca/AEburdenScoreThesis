source("R/models_and_tests/model_computer.R")

shorter_gap_time_file <- "results/shorter_gap_times/Scenario_2_k_1.5_s_0.5.RData"
longer_event_duration_file <- "results/longer_event_durations/Scenario_3_k_1.5_l_3.5.RData"
equal_file <- "results/longer_event_durations/Scenario_3_k_1.5_l_1.RData"

file <- longer_event_duration_file
# scenario <- "shorter"

model_computer <- load_model_computer(file)

plot_name <- paste0(tools::file_path_sans_ext(basename(file)),".pdf")

print(p_value_plot(model_computer,save=plot_name ,models_to_exclude = c("log_anova_c_10000", "tweedie_var_power_1.5_link_power_0")))
