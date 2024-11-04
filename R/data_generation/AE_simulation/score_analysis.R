library(ggplot2)

#' param list of trials
#' return list of all scores for control and treatment
unlist_trials <- function(trials) {
  treatment_scores <- c(sapply(trials, function(t) sapply(t$treatment, function(x) x$score)))
  control_scores <- c(sapply(trials, function(t) sapply(t$control, function(x) x$score)))
  return(list(treatment = treatment_scores, control = control_scores))
}

#' param list of trials
#' return dataframe or  string with summary of trials
trials_summary <- function(trials, return = "df") {
  treatment_scores <- unlist_trials(trials)$treatment
  control_scores <- unlist_trials(trials)$control

  zero_count_treatment <- sum(treatment_scores == 0)
  zero_count_control <- sum(control_scores == 0)
  mean_treatment <- round(mean(treatment_scores),2)
  mean_control <- round(mean(control_scores),2)
  var_treatment <-  round(var(treatment_scores),2)
  var_control <-  round(var(control_scores),2)

  str <- paste0("Treatment #zeros:", zero_count_treatment, "\n",
                "Control #zeros:", zero_count_control, "\n",
                "Treatment mean:", mean_treatment, "\n",
                "Control mean:", mean_control, "\n",
                "Treatment var:", var_treatment, "\n",
                "Control var:", var_control, "\n")
  cat(str)
  if (return == "df") {
    df <- t(data.frame(
      treatment = c(zero_count_treatment, mean_treatment, var_treatment),
      control = c(zero_count_control, mean_control, var_control),
      row.names = c("zeros", "mean", "variance")
    ))
    return(df)
  }
  else {
    return(str)
  }
}

scenario_trials_scores_distribution <- function(trials, title = "" ,save_name = "") {
  unlisted_trials <- unlist_trials(trials)
  treatment_scores <- unlisted_trials$treatment
  control_scores <- unlisted_trials$control

  # Combine scores into a data frame
  scores_data <- data.frame(
    Score = c(treatment_scores, control_scores),
    Group = rep(c("Treatment", "Control"), times = c(length(treatment_scores), length(control_scores)))
  )

  # Count zeros for each group

  # Define custom breaks for the histogram
  max_score <- max(scores_data$Score, na.rm = TRUE)
  breaks <- c(0, seq(1, max_score, by = 1))
  if (max_score %% 1 == 0) {
    breaks <- c(breaks, max_score + 1)
  }
  if(title == ""){
    title <- "Distribution of Scores"
  }
  # Create the histogram using ggplot2
  p <- ggplot(scores_data, aes(x = Score, fill = Group)) +
    geom_histogram(position = "dodge", breaks = breaks) +
    labs(title = title, x = "Score", y = "Count") +
    theme_minimal() +
    scale_fill_manual(values = c("Treatment" = "blue", "Control" = "red"))

  #add trial description
  p <- p + annotate("text", x = max_score, y = Inf, label = trials_summary(trials, "str"), vjust = 1.5, hjust = 1
  )

  if (save_name != "") {
    ggsave(paste0("plots/scores/", save_name,".png"), plot = p, width = 16, height = 10, units = "cm", dpi = 600)
  }
  return(p)
}





