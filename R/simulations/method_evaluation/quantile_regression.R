library(quantreg)
library(ggplot2)
source("R/trials/trial_simulation.R")

# Load data
trials_file <- "Scenario_2_k_1.5_s_0.5.csv"
trial_data <- get_trial_data(trials_file)
quantiles <- c(0.05, 0.25, 0.5, 0.75, 0.95)

# Prepare the results dataframe
results_df <- data.frame(Trial = integer(),
                         Tau = numeric(),
                         Estimate = numeric(),
                         LowerCI = numeric(),
                         UpperCI = numeric())

# Loop through each trial and quantile
for (i in seq_along(trial_data$trials)) {
  trial <- trial_data$trials[[i]]
  for (q in quantiles) {
    # Perform quantile regression and obtain summary
    model_result <- rq(Score ~ Group, data = trial, tau = q)
    summary_model <- summary(model_result)

    # Extract the coefficient for 'Group' and its confidence intervals
    bounds <- coef(summary_model)[2, c(2,3)]
    lower_ci <- unname(bounds[1])
    upper_ci <- unname(bounds[2])

    est <-  coef(summary_model)[2, 1]

    # Append to results dataframe
    results_df <- rbind(results_df, data.frame(Trial = i, Tau = q, Estimate = est, LowerCI = lower_ci, UpperCI = upper_ci))
  }
}

head(results_df)


values <- results_df[results_df$Tau == 0.5,]

results_df[1:20,]

ordering <- order(values$Estimate)

plot(values$Estimate[ordering], type = "p", col = "blue", xlab = "Estimate Ordered Trial", ylab = "y",
     ylim=c(min(values$LowerCI),max(50)))
lines(values$LowerCI[ordering], col = "red")
lines(values$UpperCI[ordering], col = "red")


rq(Score ~ Group-1, data = trial_data$trials[[2]], tau = quantiles)
