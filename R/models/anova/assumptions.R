# Load necessary libraries
library(ggplot2)
library(car)  # For diagnostic plots
library(dplyr)


source("R/trials/trial_simulation.R" )

file_name <- "Scenario_2_k_1.5_s_0.5.csv"
score_data <- get_trial_data(file_name)
#score_data <- score_data$all_data()
score_data <- score_data$trials[[1]]
# Basic summary and visualization of data
summary(score_data)
ggplot(score_data, aes(x = Group, y = Score)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Scores by Group", y = "Score", x = "Group")

# 1. Regular ANOVA
anova_model <- aov(Score ~ Group, data = score_data)
summary(anova_model)

AIC(anova_model)

# Diagnostic plots for regular ANOVA
par(mfrow = c(2, 2))
plot(anova_model)  # Produces 4 plots including residuals

# 2. ANOVA with log transformation with various c values
c_values <- c(0.1, 0.5, 1, 5, 10)  # Modify c values as needed
results <- list()

c <- 100
#for (c in c_values) {
  score_data[, paste("log_score_plus", c, sep = "")] <- log(score_data$Score + c)

  anova_log_model <- aov(score_data[paste("log_score_plus", c, sep = "")] ~ score_data$Group)
  summary(anova_log_model)

  # Store results in a list for later review
  results[[paste("Log_ANOVA_c", c, sep = "")]] <- summary(anova_log_model)

  # Plot diagnostics for each log model
  plot_title <- paste("Diagnostic Plots for log(score+c) with c=", c, sep="")
  par(mfrow = c(2, 2))
  plot(anova_log_model, main = plot_title)
#}

AIC(anova_log_model)

# Check residuals of the last fitted model for normality
hist(residuals(anova_log_model), main = "Histogram of Residuals", xlab = "Residuals", breaks = "Sturges")

# Returning all anova summaries for log-transformed models
results

# Levene's Test for homogeneity of variances
library(car)
leveneTest(score ~ group, data = score_data)
for (c in c_values) {
  leveneTest(get(paste("log_score_plus", c, sep = "")) ~ group, data = score_data)
}
