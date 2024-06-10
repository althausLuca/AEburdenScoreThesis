# Load necessary libraries
library(tweedie)
library(statmod)
library(ggplot2)

# Load the data
file <- "../../../data/trials_/Scenario_2_k_1.5_s_0.25.csv"
data <- read.table(file, header = FALSE, sep = ",")
df.treatment <- data.frame(data[data[, 1] == "control", -1], stringsAsFactors = FALSE)
df.treatment <- data.frame(lapply(df.treatment, function(x) as.numeric(as.character(x))), stringsAsFactors = FALSE)
all_scores <- unlist(df.treatment, use.names = FALSE)

var.power <- 1.9

# Fit a Tweedie model to the treatment data
glm_tweedie <- glm(all_scores ~ 1, family = tweedie(var.power = var.power, link.power = 0))
phi_model_est <- round(summary(glm_tweedie)$dispersion,2)
# Extract model parameters
coef_estimates <- coef(glm_tweedie)
mu_est <- exp(coef_estimates[1])  # Assuming log link, back-transform

#using var(Y) = phi*mu^var.power
phi_est <- var( all_scores ) / (mu_est^var.power)
phi_est <- round(phi_est)

phi_values <- list()
phi_values[paste0("Theoretical estimate (",phi_est,")")] <- phi_est
phi_values[paste0("GLM fitted (",phi_model_est,")")]  <- phi_model_est

# Initialize data frame to collect all simulated scores
all_simulated_scores <- data.frame()

# Loop over phi values and simulate data
for (phi_label in names(phi_values)) {
  phi_value <- phi_values[[phi_label]]
  # simulated_scores <- rtweedie(length(all_scores), mu = mu_est, phi = phi_value, power = var.power)
  temp_data <- data.frame(Score = simulated_scores, Type = phi_label)
  all_simulated_scores <- rbind(all_simulated_scores, temp_data)
}

# Combine actual and simulated scores for plotting
combined_scores <- rbind(data.frame(Score = all_scores, Type = "Actual"), all_simulated_scores)

combined_scores <- combined_scores[combined_scores$Score >0 ,]

# Plot using ggplot2 with histograms showing counts directly
ggplot(combined_scores, aes(x = Score, fill = Type)) +
  geom_histogram(alpha = 0.2, position = 'identity', bins = 2000) +
  labs(title = "Histograms for Various Phi Values", x = "Score", y = "Count") +  # Correct label for Y-axis
  theme_classic() +
  scale_fill_brewer(palette = "Set1", name = "Phi Values") +
  xlim(0,200) + ylim(0,1000)


# Residual plot
#plot(residuals(glm_tweedie), type = 'h', main = "Plot of Residuals", xlab = "Index", ylab = "Residuals")
#abline(h = 0, col = "red")

# Q-Q plot of residuals
#qqnorm(residuals(glm_tweedie))
#qqline(residuals(glm_tweedie), col = "red")


#print(paste("AIC of Tweedie model:", AIC(glm_tweedie)))
