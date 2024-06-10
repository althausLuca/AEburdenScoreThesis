source("R/Scenarios.R")
library(ggplot2)
library(gridExtra)

scenario <- SCENARIOS$frequency
for (scenario in SCENARIOS) {
  data <- load_data(scenario)
  scores <- data$scores

  scores[[1]]$scores[scores[[1]]$groups == "control"]

  all_scores_treatment <- unlist(lapply(scores[1:100],
                                        function(df) df$scores[df$groups == "treatment"]))
  all_scores_control <- unlist(lapply(scores[1:100],
                                      function(df) df$scores[df$groups == "control"]))

  max_score <- max(c(all_scores_treatment, all_scores_control))

  # Calculate mean, variance, and log of variance
  mean_treatment <- mean(all_scores_treatment)
  variance_treatment <- var(all_scores_treatment)
  log_variance_treatment <- log(variance_treatment)

  mean_control <- mean(all_scores_control)
  variance_control <- var(all_scores_control)
  log_variance_control <- log(variance_control)

  # Create a histogram for treatment scores
  p1 <- ggplot(data.frame(score = all_scores_treatment), aes(x = score)) +
    geom_histogram(binwidth = 3, fill = "blue", color = "black") +
    ggtitle(paste("Distribution of Treatment Scores -", scenario$name)) +
    annotate("text", x = Inf, y = Inf, label = paste("Mean:", round(mean_treatment, 2),
                                                     "\nVariance:", round(variance_treatment, 2),
                                                     "\nLog Variance:", round(log_variance_treatment, 2)),
             hjust = 1.1, vjust = 1.1) +
    xlim(0, max_score+10) +
    xlab("Score") +
    ylab("Frequency")

  # Create a histogram for control scores
  p2 <- ggplot(data.frame(score = all_scores_control), aes(x = score)) +
    geom_histogram(binwidth = 3, fill = "red", color = "black") +
    ggtitle(paste("Distribution of Control Scores -", scenario$name)) +
    annotate("text", x = Inf, y = Inf, label = paste("Mean:", round(mean_control, 2),
                                                     "\nVariance:", round(variance_control, 2),
                                                     "\nLog Variance:", round(log_variance_control, 2)),
             hjust = 1.1, vjust = 1.1) +
    xlim(0, max_score+10) +
    xlab("Score") +
    ylab("Frequency")
  # p1
  # p2


  combined_plot <- gridExtra::grid.arrange(p1, p2, ncol = 1)
  ggsave(paste0("plots/scores/",scenario$name, "_score_distribution.png"), combined_plot, width = 10, height = 8)
}