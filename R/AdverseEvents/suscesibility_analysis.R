library("gridExtra")
source("R/AdverseEvents/event_simulation.R")
source("R/trials/trial_analysis.R")

gamma_shapes <- c(1, 2, 10)
n.samp <- 1000
#AE_def <- AE(10, 30,severity_probability=c(1/3,1/3,1/3))
AE_def.1 <- AE(3, 10, severity_probabilities = c(1 / 3, 1 / 3, 1 / 3))
AE_def.2 <- AE(5, 50, severity_probabilities = c(1 / 3, 1 / 3, 1 / 3))
AE_def.3 <- AE(10, 100, severity_probabilities = c(1 / 3, 1 / 3, 1 / 3))
AE_def.4 <- AE(50, 1000, severity_probabilities = c(1 / 3, 1 / 3, 1 / 3))

AE_definitions <- list(AE_def.1, AE_def.2, AE_def.3, AE_def.4)

plot_list <- list()
for (shape in gamma_shapes) {
  susceptibilities <- rgamma(n.samp, shape, scale = 1 / shape)
  h <- hist_plot(susceptibilities, title = paste("Susc. hist shape:", shape))
  plot_list <- c(plot_list, list(h))
  for (AE_def in AE_definitions) {
    scores <- c()
    for (susceptibility in susceptibilities) {

      simulated_AE <- simulate_event(AE_def, 180, susceptibility = susceptibility)
      # simulated_AE$info
      simulated_AE$score
      scores <- c(scores, simulated_AE$score)
    }
    h <- hist_plot(scores, title = ifelse(shape==gamma_shapes[1], paste0("Score hist ", AE_def$event_duration ,", ", AE_def$time_between_events),""), zero_count = TRUE, mean = TRUE)
    plot_list <- c(plot_list, list(h))
  }
}

do.call("grid.arrange", c(plot_list, ncol = length(AE_definitions) + 1))

