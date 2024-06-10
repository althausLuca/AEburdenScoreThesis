source("R/AdverseEvents/helpers.R")
source("R/scenario_configuration.R")

library(ggplot2)
library(gridExtra)


plot_text_size <- 5
#'plot a single AE
plot_ae <- function(ae_data, color = "green", max_time = 180, by = 10, save = "") {
  death <- DEATH %in% ae_data$info$Severity


  ae_data$info$Severity <- severities_to_values(ae_data$info$Severity)

  g <- ggplot(ae_data$info, aes(xmin = Start, xmax = End, ymin = 0, ymax = Severity)) +
    geom_rect(fill = color, color = "black") +
    scale_x_continuous(name = "Treatment duration (days)",
                       limits = c(0, max_time + 20),
                       breaks = seq(0, max_time + 20, by = 50), expand = c(0, 0)) +
    scale_y_continuous(name = "Severity",
                       limits = c(0, 5),
                       breaks = 0:5, expand = c(0, 0)) +
    theme_bw() +
    theme(legend.position = "none", plot.margin = margin(0.2, 0.5, 0.2, 0.2, "cm"))
  #add score to plot
  g <- g + geom_text(aes(x = 5, y = 4.5, label = paste("Score:", round(ae_data$score,2))), size = 4, hjust = 0)
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
    ggsave(paste0("plots/", save), plot = g, width = 10, height = 6, units = "cm", dpi = 600)

  }

  return(g)
}

plot_AEs <- function(AEs, one_plot = TRUE) {
  plot_list <- list()
  for (ae in AEs) {
    g <- plot_ae(ae)
    plot_list <- c(plot_list, list(g))
  }
  if (one_plot == TRUE) {
    do.call("grid.arrange", c(plot_list, ncol=1))
  }
  else {
    for (g in plot_list) {
      g
    }
  }

  invisible(plot_list)
}