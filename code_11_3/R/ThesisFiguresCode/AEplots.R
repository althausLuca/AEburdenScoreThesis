library(ggplot2)

plot_folder <- "plots/ae_figures/"
plot_text_size <- 3.5

plot_ae <- function(ae_data, color = "green", max_time = 180, by = 10, save = "", death = FALSE, score=!death) {
  g <- ggplot(ae_data$info, aes(xmin = Start, xmax = End, ymin = 0, ymax = Severity)) +
    geom_rect(fill = color, color = "black") +
    scale_x_continuous(name = "Treatment duration (days)",
                       limits = c(0, max_time + 20),
                       breaks = seq(0, max_time + 20, by = 50), expand = c(0, 0)) +
    scale_y_continuous(name = "Severity",
                       limits = c(0, 5.2),
                       breaks = 0:5, expand = c(0, 0)) +
    theme_bw() +
    theme(legend.position = "none", plot.margin = margin(0.2, 0.5, 0.2, 0.2, "cm"))
  #add score to plot
  if (score == TRUE) {
    g <- g + geom_text(aes(x = 5, y = 5, label = paste("Score:", ae_data$score)), size = 4, hjust = 0)
  }
  ## add finish line with label
  if (death == TRUE) {
    death_time <- tail(ae_data$info$Start, n = 1)
    g <- g + geom_vline(xintercept = death_time, color = "firebrick1", size = 1.5)
    g <- g + geom_text(aes(x = death_time - 4, y = 4.2, label = paste("Death")), size = plot_text_size, hjust = 1, color = "red")

    g <- g + geom_vline(xintercept = max_time, linetype = "dashed", color = "black")
    g <- g + geom_text(aes(x = max_time - 4, y = 4.5, label = paste("End of Trial")), size = plot_text_size, hjust = 1)
  }
  else {
    g <- g + geom_text(aes(x = max_time - 4, y = 4.5, label = paste("Completed")), size = plot_text_size, hjust = 1)
    g <- g + geom_vline(xintercept = max_time, linetype = "dashed", color = "black")

  }
  if (save != "") {
    ggsave(paste0(plot_folder, save), plot = g, width = 10, height = 6, units = "cm", dpi = 600)

  }

  return(g)
}

ae_data <- list(
  info = data.frame(
    Severity = c(2, 2),
    Start = c(10, 60),      # Start position of each AE
    End = c(10 + 30, 60 + 10)  # End position is Start position plus Duration
  ),
  n_events = 2,
  score = 2 * 30 + 2 * 10
)

ae_data_2 <- list(
  info = data.frame(
    Severity = c(1, 3, 2),
    Start = c(40, 70, 120),      # Start position of each AE
    End = c(40, 70, 120) + c(20, 10, 30)  # End position is Start position plus Duration
  ),
  n_events = 3,
  score = 2 * 20 + 4 * 10 + 3 * 30
)

plot_ae(ae_data, save = "ae11.png", color = "limegreen")
plot_ae(ae_data_2, color = "tomato", save = "ae12.png")


ae_data_death.1 <- list(
  info = data.frame(
    Severity = c(3, 3, 0),
    Start = c(45, 60, 120),      # Start position of each AE
    End = c(45, 60, 120) + c(5, 15, 60)  # End position is Start position plus Duration
  ),
  n_events = 3,
  score = 2 * 20 + 4 * 10 + 4 * 60
)
plot_ae(ae_data_death.1, color = "limegreen", save = "ae_data_death.1.png", death = TRUE)

ae_data_death.2 <- list(
  info = data.frame(
    Severity = c(2, 3, 0),
    Start = c(10, 60, 120),      # Start position of each AE
    End =  c(10, 60, 120)   + c(20, 60, 60)  # End position is Start position plus Duration
  ),
  n_events = 3,
  score = 2 * 20 + 3 * 60 + 4 * 60
)

plot_ae(ae_data_death.2, color = "tomato", save = "ae_data_death.2.png", death = TRUE)
