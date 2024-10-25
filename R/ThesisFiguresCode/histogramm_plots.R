source("R/trials/trial_simulation.R")
source("R/AdverseEvents/AdverseEvent.R")

source("R/trials/analysis/plots.R")

file_path <- "plots/trials/trials_histogramms_/"
dir.create(file_path , recursive = TRUE)


control_AEs <- list(
  AE(3, 1500, severity_probabilities = c(0.6, 0.3, 0.1)),
  AE(7, 1500, severity_probabilities = c(0.3, 0.6, 0.1)),
  AE(3, 1500 / 2, severity_probabilities = c(0.6, 0.3, 0.1))
)

k_d <- 3
k_s <- 1.5

duartion_shape_ <- k_d

set.seed(7)
control_scores <- simulate_group(control_AEs, size = 10000, susceptibility_parameter = list("gamma", k_s), max_time = 180)

scores <- control_scores$scores
n_events <- control_scores$n_events


file_name <- paste0(file_path , "single_hist_and_box_plot_", "k_d_", k_d, "_k_s_", k_s, ".pdf")

hist_and_box_plot(scores, n_events, hist_y_lim = 250, hist_lim = 100, file_name = file_name ,save=TRUE,  bin_width = 0.5)



control_AEs <- list(
  AE(3, 1500, severity_probabilities=c(0.6, 0.3, 0.1)),
  AE(7, 1500, severity_probabilities=c(0.3 , 0.6, 0.1)),
  AE(3, 1500/2, severity_probabilities= c(0.6, 0.3, 0.1))
)

k_d <- 9
k_s <- 2.5

duartion_shape_ <- k_d

set.seed(7)
control_scores <- simulate_group(control_AEs, size = 10000, susceptibility_parameter = list("gamma", k_s), max_time = 180)

scores <- control_scores$scores
n_events <- control_scores$n_events
file_name <- paste0(file_path,"single_hist_and_box_plot_", "k_d_", k_d, "_k_s_", k_s, ".pdf")

hist_and_box_plot(scores, n_events, hist_y_lim = 250, hist_lim = 100, box_lim = 180 , file_name = file_name ,save=TRUE, bin_width=0.5)






control_AEs <- list(
  AE(3, 1500, severity_probabilities=c(0.6, 0.3, 0.1)),
  AE(7, 1500, severity_probabilities=c(0.3 , 0.6, 0.1)),
  AE(3, 1500/2, severity_probabilities= c(0.6, 0.3, 0.1))
)

  set.seed(6)

  k_d <- 9
  k_s <- 1.5

  duartion_shape_ <- k_d

  control_scores <- simulate_group(control_AEs, size = 10000, susceptibility_parameter = list("gamma", k_s), max_time = 180)

  scores <- control_scores$scores
  n_events <- control_scores$n_events

file_name <- paste0( file_path,"default_control.pdf")
hist_and_box_plot(scores, n_events, hist_y_lim = 250, hist_lim = 100, save= TRUE, file_name =file_name , bin_width=0.5)



### Experiemental Group settings

# shorter gap times
c_g <- 0.5

experimental_AEs <- list(
  AE(3, 1500*c_g, severity_probabilities=c(0.6, 0.3, 0.1)),
  AE(7, 1500*c_g, severity_probabilities=c(0.3 , 0.6, 0.1)),
  AE(3, 1500/2*c_g, severity_probabilities= c(0.6, 0.3, 0.1))
)


set.seed(7)
k_d <- 9
k_s <- 1.5
duartion_shape_ <- k_d

experimental_AEs <- simulate_group(experimental_AEs, size = 10000, susceptibility_parameter = list("gamma", k_s), max_time = 180)

file_name <- paste0(file_path , "experiemental_shorter_gap_times.pdf")

hist_and_box_plot(experimental_AEs$scores, experimental_AEs$n_events, hist_y_lim = 250, hist_lim = 100, save= TRUE, file_name =file_name , bin_width=0.5)


# longer event durations

c_d <- 3.5

experimental_AEs <- list(
  AE(3*c_d, 1500, severity_probabilities=c(0.6, 0.3, 0.1)),
    AE(7*c_d, 1500, severity_probabilities=c(0.3 , 0.6, 0.1)),
    AE(3*c_d, 1500/2, severity_probabilities= c(0.6, 0.3, 0.1))
)

set.seed(7)
k_d <- 9
k_s <- 1.5
duartion_shape_ <- k_d

experimental_AEs <- simulate_group(experimental_AEs, size = 10000, susceptibility_parameter = list("gamma", k_s), max_time = 180)

file_name <- paste0( file_path , "experiemental_longer_event_durations.pdf")

hist_and_box_plot(experimental_AEs$scores, experimental_AEs$n_events, hist_y_lim = 250, hist_lim = 100, save= TRUE, file_name =file_name , bin_width=0.5)


# higher severity levels

experimental_AEs <- list(
  AE(3, 1500, severity_probabilities=c(0.3 , 0.6, 0.1)),
  AE(7, 1500, severity_probabilities=c(0.2, 0.3 , 0.5)),
  AE(3, 1500/2, severity_probabilities= c(0.3 , 0.6, 0.1))
)

set.seed(7)
k_d <- 9
k_s <- 1.5

duartion_shape_ <- k_d

experimental_AEs <- simulate_group(experimental_AEs, size = 10000, susceptibility_parameter = list("gamma", k_s), max_time = 180)

file_name <- paste0(file_path , "experiemental_higher_severity_levels.pdf")

hist_and_box_plot(experimental_AEs$scores, experimental_AEs$n_events, hist_y_lim = 250, hist_lim = 100, save= TRUE, file_name =file_name , bin_width=0.5)
