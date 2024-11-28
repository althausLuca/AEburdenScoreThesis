source("R/evaluation/config.R")


longer_trials <- load_longer_trials()

longer_trials$AEs

trial <- trial_as_df(longer_trials$trials[[37]])
max(trial$Score)
mean(trial$Score)
summary(trial$Score)

trial$Group[trial$Group == "treatment"] <- "Experimental"
trial$Group[trial$Group == "control"] <- "Control"

c = 0.001

# log transformed score data
trial$Log_score <- log(trial$Score + c)

# fit a lm model with the score and log score

lm_model <- lm(trial$Score ~ trial$Group)
lm_log_model <- lm(trial$Log_score ~ trial$Group)

# compare the treatment effect p values
summary(lm_model)$coefficients[2,4]
summary(lm_log_model)$coefficients[2,4]


# Load required packages
library(ggplot2)
library(dplyr)
library(gridExtra)

# Assuming your data frame is named trial and has the columns Score, Log_score, and Group

# Original log transformation with c = 0.001
c1 = 0.001
trial$Log_score_0.001 <- log(trial$Score + c1)

# Additional log transformation with c = 1
c2 = 1
trial$Log_score_1 <- log(trial$Score + c2)

# Create the histogram for Score
p1 <- ggplot(trial, aes(x = Score, fill = Group)) +
  geom_histogram(alpha = 0.6, position = "identity", binwidth = 2) +
  labs(title = "", x = "Score", y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

# Create the histogram for Log_score with c = 0.001
p2 <- ggplot(trial, aes(x = Log_score_0.001, fill = Group)) +
  geom_histogram(alpha = 0.6, position = "identity", binwidth = 0.3) +
  labs(title = "", x = "Log(Score+0.001)", y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

# Create the histogram for Log_score with c = 1
p3 <- ggplot(trial, aes(x = Log_score_1, fill = Group)) +
  geom_histogram(alpha = 0.6, position = "identity", binwidth = 0.1) +
  labs(title = "", x = "Log(Score+1)", y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

# Combine the plots
grid.arrange(p1, p3, p2, nrow = 3)



lm_model <- lm(trial$Score ~ trial$Group)
lm_log_model_1 <- lm(trial$Log_score_1 ~ trial$Group)
lm_log_model_0.001 <- lm(trial$Log_score_0.001 ~ trial$Group)


# save each plot to plots/trials/log_variation
dir.create("plots/experiments/log_variation", showWarnings = FALSE , recursive = TRUE)

ggsave("plots/experiments/log_variation/histogram_score.pdf", p1, width = 10, height = 2, units = "in")
ggsave("plots/experiments/log_variation/histogram_log_score_1.pdf", p3, width = 10, height = 2, units = "in")
ggsave("plots/experiments/log_variation/histogram_log_score_0.001.pdf", p2, width = 10, height = 2, units = "in")

# compare the treatment effect p values
summary(lm_model)$coefficients[2,]
summary(lm_log_model_1)$coefficients[2,]
summary(lm_log_model_0.001)$coefficients[2,]

