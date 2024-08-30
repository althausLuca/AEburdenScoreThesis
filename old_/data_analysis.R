library(jsonlite)

#' Generate model plots from given data
#' @param data A list of dataframes containing the data for each model
#' @param models A list of model names
#' @param parameters A list of parameters for each model to plot
plot_models <- function(data, models, parameters) {
  measure_list <- data

  for (i in seq_along(models)) {
    parameter_values <- list()

    model <- models[i]
    parameter <- parameters[i]

    p <- measure_list[[model]][[parameter]]
    parameter_values[[paste(model, parameter)]] <- p

    # Compute confidence intervals
    lower_bound <- quantile(unlist(parameter_values), 0.025)
    upper_bound <- quantile(unlist(parameter_values), 0.975)
    # Create a boxplot for the current combination of model and parameter
    boxplot(p, main = paste("Plot of", parameter, "for", model, "CI:(", lower_bound, ",", upper_bound, ")"), xlab = "Model")
  }
}

#' Generate model plots from given data
#' @param data A list of dataframes containing the data for each model
#' @param models A list of model names
#' @param parameters A list of parameters for each model to plot the distribution
plot_distritubtions <- function(data, models, parameters) {
  measure_list <- data
  plots <- list()

  for (i in seq_along(models)) {
    model <- models[i]
    parameter <- parameters[i]

    # Extract the data for the current model and parameter
    data <- measure_list[[model]][[parameter]]
    data <- unlist(data)

    # Create a histogram plot of the data
    hist_plot <- hist(data, main = paste("Histogram of", parameter, "for", model), xlab = "Values")

    # Store the plot in the list
    plots[[paste(model, parameter)]] <- hist_plot
  }
  #return(plots)

}

plot_estimate_by_std <- function(data) {
  lm_effects <- unlist(data$lm$estimate)
  lm_std_err <- unlist(data$lm$std_err)


  tweedy_effects_1_5 <- unlist(data$tweedy_1.5_0$estimate)
  tweedy_std_err_1_5 <- unlist(data$tweedy_1.5_0$std_err)

  quantile_effects <- unlist(data$quantile_regression$estimate)
  quantile_std_err <- unlist(data$quantile_regression$std_err)

  lm_effects_by_std <- lm_effects / lm_std_err
  tweedy_effects_by_std_1_5 <- tweedy_effects_1_5 / tweedy_std_err_1_5
  quantile_effects_by_std <- quantile_effects / quantile_std_err

  #sort the values
  lm_effects_by_std <- sort(lm_effects_by_std)
  tweedy_effects_by_std_1_5 <- sort(tweedy_effects_by_std_1_5)
  quantile_effects_by_std <- sort(quantile_effects_by_std)

  #plot the values
  plot(lm_effects_by_std, type = "l", col = "red", xlab = "Standardized effect", main = "Cumulative probability of standardized effect")
  lines(tweedy_effects_by_std_1_5, col = "blue")
  lines(quantile_effects_by_std, col = "green")

  #add legend
  legend("topright", c("lm", "tweedy", "quantile"), col = c("red", "blue", "green"), lty = 1, cex = 0.8)

}

p_value_plot <- function(data) {
  # set plot options to two plots next to each other
  par(mfrow = c(1, 2))
  lm_p_value <- unlist(data$lm$p_value)
  tweedy_p_value_1_5 <- unlist(data$tweedy_1.5_0$p_value)
  quantile_p_value <- unlist(data$quantile_regression$p_value)

  #sort the values
  lm_p_value_sorted <- sort(lm_p_value)
  tweedy_p_value_1_5_sorted <- sort(tweedy_p_value_1_5)
  quantile_p_value_sorted <- sort(quantile_p_value)

  #plot the values
  plot(lm_p_value_sorted, type = "l", col = "red", xlab = "p-value" , ylim=c(0,0.1))
  lines(tweedy_p_value_1_5_sorted, col = "blue")
  lines(quantile_p_value_sorted, col = "green")

  #add legend
  legend("topright", c("lm", "tweedy", "quantile"), col = c("red", "blue", "green"), lty = 1, cex = 0.8)


  ## plot highest p values
  lm <- ifelse(lm_p_value > tweedy_p_value_1_5 & lm_p_value > quantile_p_value, 1 , 0)
  tweedy <- ifelse(tweedy_p_value_1_5 > lm_p_value & tweedy_p_value_1_5 > quantile_p_value, 1 , 0)
  quantile <- ifelse(quantile_p_value > lm_p_value & quantile_p_value > tweedy_p_value_1_5, 1 , 0)

  #vizualize the highest p values
    plot(lm, type = "p", col = "red", xlab = "p-value", main = paste0("Highest p-value for each model")
      , sub = paste0("lm:", sum(lm), ", tweedy:", sum(tweedy), ", quantile", sum(quantile)))
    points(tweedy, col = "blue")
    points(quantile, col = "green")

    #add legend
    legend("topright", c("lm", "tweedy", "quantile"), col = c("red", "blue", "green"), lty = 1, cex = 0.8)

  #restore plot option
  par(mfrow = c(1, 1))

  return(list(lm = lm_p_value, tweedy = tweedy_p_value_1_5, quantile = quantile_p_value))
}

## time line plot
plot_timeline <- function(patient_i, trial, consultations = 4 * 7) {
  # Get the data for the specified patient
  durations <- trial$durations[[patient_i]]
  severities <- trial$severities[[patient_i]]
  event_starts <- trial$event_starts[[patient_i]]
  treatment_group <- trial$group_events$group[patient_i]

  # Calculate the end times for each event
  event_ends <- event_starts + durations
  # Create the plot
  par(xpd = TRUE)
  max_time <- 7 * 4 * 6
  plot(0, type = "n", xlim = c(0, max_time), ylim = c(0, 4),
       xlab = "time (days)", ylab = "severity", main = paste("Timeline for Patient", patient_i, "(", treatment_group, ")"), axes = FALSE)
  axis(1, at = seq(0, max_time, by = 7))
  axis(2) # Add the x-axis

  # add consultation vertical lines
  for (i in seq(0, max_time, by = consultations)) {
    segments(i, 0, i, 4, lwd = 2, col = "grey", lty = "dotted")
  }


  # Plot each event as a segment, with y-position determined by severity
  for (i in seq_along(event_starts)) {
    segments(min(event_starts[i], max_time), severities[i], min(event_ends[i], max_time), severities[i], lwd = 5)
    # text(mean(c(event_starts[i], event_ends[i])), severities[i]+0.1, labels = severities[[i]], cex = 0.8)
  }
  #add legend above the plot
  legend(legend = c("Consultations", "AEs"), inset = c(1, 0),
         col = c("grey", "black"), lty = c("dotted", "solid"), lwd = c(2, 5), cex = 0.8, x = 0, y = 4.2,
         box.lty = 0, box.lwd = 0, bg = "white", xjust = 0)
}
