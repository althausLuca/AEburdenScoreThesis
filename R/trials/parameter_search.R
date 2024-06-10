# File to analyzed single trials on a gorup level

rm(list = ls())

source("R/AdverseEvents/AdverseEvents.R")
source("R/trials/trial_simulation.R")
source("R/Scenarios.R")
library(tidyverse)

n_sim <- 10000
Constant <- c(1, 0, 0)
k <- 5
shapes <- c(9)

long <- 150
duartion_s <- 3
AEs <- list(AE(duartion_s, long, MOSTLY_MILD),
            AE(7, long, MOSTLY_MODERATE),
            AE(duartion_s, long / 2, MOSTLY_MILD))

duartion_shape_ <- -1

results <- list()
n_events <- list()
for (duartion_shape in shapes) {
  duartion_shape_ <<- duartion_shape
  group_values <- simulate_group(AEs, size = n_sim, susceptibility_parameter = list("gamma", k), max_time = 180)
  group_scores <- group_values$scores
  event_count <- group_values$n_events
  zero_percent <- mean(group_scores == 0)
  mean_score <- mean(group_scores[event_count == 1])
  max_score <- max(group_scores)
  var_score <- var(group_scores)
  median_score <- median(group_scores[event_count == 1])
  results[[as.character(duartion_shape)]] <- group_scores
  n_events[[as.character(duartion_shape)]] <- event_count
}

#p of seeing no event excluding subse
1 - (1 - exp(-1 / 1500 * 180)) *
  (1 - exp(-1 / 1500 / 2 * 180)) *
  (1 - exp((-1 / 1500 * 180)))

for (shape in names(results)) {
  score_shape <- results[[shape]]
  zero_percent <- round(mean(score_shape == 0) * 100, 3)
  n_event <- n_events[[shape]]
  n_event <- n_event[score_shape > 0]

  mean_with_zero <- round(mean(score_shape), 1)
  sd_with_zero <- round(sd(score_shape), 2)
  median_with_zero <- round(median(score_shape), 1)
  nn_percentile_with_zero <- round(quantile(score_shape, 0.99), 1)

  score_shape <- score_shape[score_shape > 0]

  mean <- round(mean(score_shape), 1)
  sd <- round(sd(score_shape), 2)
  median <- round(median(score_shape), 1)
  nn_percentile <- round(quantile(score_shape, 0.99), 1)


  score_shape_event1 <- score_shape[n_event == 1]
  score_shape_event2 <- score_shape[n_event <= 2]

  file_name <- paste0("plots/trials/", shape, "_k_", k, "_g_", long, "_d_", duartion_s, ".png")
  png(file_name, width = 500, height = 800)

  # Set plot layout
  layout(mat = matrix(c(1, 2), 2, 1, byrow = TRUE), height = c(7, 2))

  # Plotting the first histogram
  par(mar = c(4, 4, 2, 1))
  h <- hist(score_shape, breaks = 500, xlim = c(0, 50), col = rgb(0.2, 0.8, 0.5, 0.5), border = F, main = paste0("Event Duration Shape: ", shape), xlab = "Score")
  h
  h$breaks

  hist(score_shape_event2, breaks = h$breaks, xlim = c(0, 50), col = "blue", border = F, add = TRUE)
  hist(score_shape_event1, breaks = h$breaks, xlim = c(0, 50), col = rgb(0.8, 0.4, 0.4, 1), border = F, add = TRUE)

  # Add a legend to the top right
  legend("topright", legend = c("1 Event", "2 Events", ">= 3 Events"),
         fill = c(rgb(0.8, 0.4, 0.4, 1), "blue", rgb(0.2, 0.8, 0.5, 0.5)), border = F, bty = "n")


  info <- c(AEs_str(AEs), paste0("k: ", k))

  legend("right", legend = info,
         fill = c(rgb(0, 0, 0, 0)), border = F, bty = "n")


  # Plotting the boxplot
  box_plot_main <- paste0("Mean: ", round(mean(score_shape), 1), "  SD: ", round(sd(score_shape), 2), "  Median: ", round(median(score_shape), 1),
                          "  No AE (%): ", zero_percent)
  par(mar = c(2, 4, 1.5, 1))
  boxplot(score_shape, horizontal = TRUE, col = rgb(0.8, 0.8, 0, 0.5),
          main = box_plot_main,
          xlab = "Value", cex.main = 1)

  dev.off()
  table_entry <- paste0(shape ," & ",
                        k," & ",
                        long, " & ",
                        zero_percent, " & ",
                        mean,"/", mean_with_zero, " & ",
                        sd, "/", sd_with_zero, " & ",
                        median, "/", median_with_zero, " & ",
                        nn_percentile, "/", nn_percentile_with_zero, " \\")
  print(table_entry)
}



