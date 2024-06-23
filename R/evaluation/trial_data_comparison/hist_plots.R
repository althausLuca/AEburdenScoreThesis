source("R/trials/trial_simulation.R")
source("R/Scenarios.R")
source("R/trials/analysis/plots.R")


n_sim <- 5000
n_subjects <- 100 # per group
k <- 1.5

# generate data
scenario_name <- "Scenario_2"
shorter <- 0.5
scenario <- load_scenario(scenario_name, shorter = shorter)
shorter_control.group <- simulate_group(scenario$control, size = n_sim, susceptibility_parameter = list("gamma", k), max_time = 180)
shorter_treatment.group <- simulate_group(scenario$treatment, size = n_sim, susceptibility_parameter = list("gamma", k), max_time = 180)

scenario_name <- "Scenario_3"
longer <- 3.5
scenario <- load_scenario(scenario_name, longer = longer)
longer_control.group <- simulate_group(scenario$control, size = n_sim, susceptibility_parameter = list("gamma", k), max_time = 180)
longer_treatment.group <- simulate_group(scenario$treatment, size = n_sim, susceptibility_parameter = list("gamma", k), max_time = 180)

scenario_name <- "Scenario_4"
scenario <- load_scenario(scenario_name)

more_severe_control.group <- simulate_group(scenario$control, size = n_sim, susceptibility_parameter = list("gamma", k), max_time = 180)
more_severe_treatment.group <- simulate_group(scenario$treatment, size = n_sim, susceptibility_parameter = list("gamma", k), max_time = 180)

allscores <- list(shorter_control.group$scores, shorter_treatment.group$scores,
                   longer_control.group$scores, longer_treatment.group$scores,
                   more_severe_control.group$scores, more_severe_treatment.group$scores)

box_lim <- max(unlist(allscores))+10

bin_witdh <- 1

hist_ylim <- max(sapply(allscores, function(scores){
  max(sapply(1:500 , function(i) sum(scores > i & scores <= i+bin_witdh)))
})) + 10




plot_folder <- ""

## shorter scenario
dev.off()
hist_and_box_plot(shorter_treatment.group$scores, shorter_treatment.group$n_events
  , file_name = paste0(plot_folder,"scenario_2_treatment_s_", shorter, ".pdf") , main="Experimental Group"
  ,hist_lim = 100 , box_lim = box_lim , hist_y_lim = hist_ylim , bin_width = bin_witdh)
hist_and_box_plot(shorter_control.group$scores, shorter_control.group$n_events , box_lim = box_lim  , hist_y_lim = hist_ylim
  , file_name = paste0(plot_folder,"scenario_2_control_s_", shorter, ".pdf")
  , hist_lim = 100 , main="Control Group", bin_width = bin_witdh)


## longer scenario
dev.off()
hist_and_box_plot(longer_treatment.group$scores, longer_treatment.group$n_events
  , file_name = paste0(plot_folder,"scenario_3_treatment_l_", longer, ".pdf") , main="Experimental Group"
  ,hist_lim = 100 , box_lim = box_lim
  , hist_y_lim = hist_ylim , bin_width = bin_witdh)
hist_and_box_plot(longer_control.group$scores, longer_control.group$n_events , box_lim = box_lim
  , hist_y_lim = hist_ylim
  , file_name = paste0(plot_folder,"scenario_3_control_l_", longer, ".pdf")
  , hist_lim = 100 , main="Control Group", bin_width = bin_witdh)


## more severe events scenario
dev.off()

hist_and_box_plot(more_severe_control.group$scores, more_severe_control.group$n_events
  , file_name = paste0(plot_folder,"scenario_4_treatment", ".pdf") , main="Treatment Group"
  ,hist_lim = 100, hist_y_lim = hist_ylim, bin_width = bin_witdh)
hist_and_box_plot(more_severe_treatment.group$scores, more_severe_treatment.group$n_events
  , file_name = paste0(plot_folder,"scenario_4_control", ".pdf")
  , hist_lim = 100 , main="Experimental Group", hist_y_lim = hist_ylim, bin_width = bin_witdh)