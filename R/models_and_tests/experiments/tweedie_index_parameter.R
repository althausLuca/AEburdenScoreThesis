  source("R/Scenarios.R")
  source("R/methods/run_methods.R")
  library(ggplot2)


  scenario <- SCENARIOS$severity
  data <- load_data(scenario)
  data <- data[1:500,]
  # Create a CSV file in data/experiments/tweedie_index_parameter_{ scenario$name}.csv
  file_path <- paste0("data/experiments/tweedie_index_parameter_", scenario$name, ".csv")

  # Prepare a dataframe to store the results
  results <- data.frame(p_max = numeric(), L_max = numeric() , phi_max = numeric() )

  for(score_df in data$scores){
    prof <- tweedie.profile(score_df$scores ~ score_df$groups, data=score_df, p.vec = seq(1, 2.5, by = 0.1), link.power=0)
    p_max <- prof$p.max
    L_max <- prof$L.max
    phi_max <- prof$phi.max

    # prof_1 <- tweedie.profile(score_df$scores ~ score_df$groups, data=score_df, p.vec = seq(1, 2.5, by = 0.1), link.power=1)
    # p_max_1 <- prof_1$p.max
    # L_max_1 <- prof_1$L.max

    # Append p.max and L.max to results dataframe
    results <- rbind(results, data.frame(p_max = p_max, L_max = L_max , phi_max = phi_max ))
  }

  # Write the results to the CSV file
  write.csv(results, file_path, row.names = FALSE)


  # plot the distribution and save the plot to plots/experiments/tweedie_index_parameter_{scenario$name}.png
  plot_path <- paste0("plots/experiments/tweedie_index_parameter_", scenario$name, ".png")
  ggplot() +
    geom_histogram(data = results, aes(x = p_max, fill = "linkpower 0"), breaks = seq(1, 2.5, by = 0.1), color = "black", alpha = 0.5) +
    # geom_histogram(data = results, aes(x = p_max_1, fill = "linkpower 1"), breaks = seq(1, 2.5, by = 0.1), color = "black", alpha = 0.5) +
    scale_fill_manual(values = c("linkpower 0" = "blue"), name = "Link Power") +
    xlab("p.max value") +
    ylab("Frequency") +
    ggtitle(paste("Distribution of p.max -", scenario$name)) +
    theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10))

  # Save the plot
  ggsave(plot_path, width = 10, height = 8)


  (results$L_max - results$L_max_1 > 0)