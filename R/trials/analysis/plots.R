hist_and_box_plot <- function(scores, n_events = NULL, save = TRUE, include_zero = FALSE,
                              file_name = "plot.png", hist_lim = 50, main = "", hist_y_lim = NULL,
                              box_lim = max(scores) + 10, bin_width = NA) {

  nbins <- if (!is.na(bin_width)) {
    seq(0, max(scores) + bin_width, by = bin_width)
  } else {
    500
  }

  zero_percent <- round(mean(scores == 0) * 100, 3)
  mean <- round(mean(scores), 1)
  median <- round(median(scores), 1)
  sd <- round(sd(scores), 2)

  if (!include_zero) {
    scores <- scores[scores > 0]
    n_events <- n_events[scores > 0]
  }

  # mean median and sd without 0s
  mean_w_zero <- round(mean(scores[scores > 0]), 1)
  median_w_zero <- round(median(scores[scores > 0]), 1)
  sd_w_zero <- round(sd(scores[scores > 0]), 2)

  if (!is.null(n_events)) {
    scores_event1 <- scores[n_events == 1]
    scores_event2 <- scores[n_events <= 2]
  }

  if (save) {
    file_name <- paste0("plots/trials/", file_name)
    pdf(file_name, width = 7, height = 10)
  }

  if (!is.null(hist_y_lim)) {
    hist_y_lim <- c(0, hist_y_lim)
  }
  # Set plot layout
  layout(mat = matrix(c(1, 2), 2, 1, byrow = TRUE), height = c(7, 2))
  # Plotting the first histogram
  par(mar = c(4, 4, 2, 1))
  h <- hist(scores, breaks = nbins, xlim = c(0, hist_lim), ylim = hist_y_lim,
            col = rgb(0.2, 0.8, 0.5, 0.5), border = F, main = "", xlab = NA, ylab = NA, cex = 1.5)
  h

  if (!is.null(n_events)) {
    hist(scores_event2, breaks = h$breaks, xlim = c(0, hist_lim), ylim = hist_y_lim,
         col = "blue", border = F, add = TRUE)
    hist(scores_event1, breaks = h$breaks, xlim = c(0, hist_lim), ylim = hist_y_lim,
         col = rgb(0.8, 0.4, 0.4, 1), border = F, add = TRUE)

    # Add a legend to the top right
    legend("topright", legend = c("1 Event", "2 Events", ">= 3 Events"),
           fill = c(rgb(0.8, 0.4, 0.4, 1), "blue", rgb(0.2, 0.8, 0.5, 0.5)), border = F, bty = "n", cex = 1.4)
  }
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

  mtext(main, side = 3, line = 0.5, cex = 1.5)
  mtext("Score", side = 1, line = 2, cex = 1.4)
  mtext("Frequency", side = 2, line = 2, cex = 1.4)

  # Plotting the boxplot
  box_plot_main <- paste0("Mean: ", mean, "/", mean_w_zero, "  SD: ", sd, "/", sd_w_zero, "  Median: ", median, "/", median_w_zero)

  par(mar = c(5.5, 4, 1.5, 2))
  boxplot(scores, horizontal = TRUE, col = rgb(0.8, 0.8, 0, 0.5),
          main = "",
          xlab = "",
          ylim = c(0, box_lim), cex = 1.2)

  mtext("Scores Boxplot", side = 3, line = 0.5, cex = 1.2)

  mtext(paste0("No AE (%): ", zero_percent), side = 1, line = 2.9, cex = 1.7, font = 2)
  mtext(box_plot_main, side = 1, line = 4.6, cex = 1.5, font = 2)

  if (save) {
    dev.off()
  }
  # reset layout
  par(mfrow = c(1, 1))
  layout(1)
}
