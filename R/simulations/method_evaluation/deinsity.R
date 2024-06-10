library(tweedie)
library(statmod)


source("R/trials/trial_simulation.R")


file <- "Scenario_3_k_1.5_l_3.5.csv"
trial_data <- load_trial_data(file)$all_data()
trial_data <- load_trial_data(file)$trials[[1]]
# tweedie
var.power <- 1.85
link_power = 1

profile_result <- tweedie.profile(trial_data$Score ~ trial_data$Group, link.power = link_power, fit.glm=F , xi.vec
                = seq(1.1, 2, by=0.1))

optimal_xi <- profile_result$xi.max

var.power <- optimal_xi

glm_tweedie <- glm(Score ~ Group, data=trial_data, family = tweedie(var.power = var.power, link.power = 1))

mu_tweedie <- coef(glm_tweedie)[1]
mu_tweeide_treatment <-  sum(coef(glm_tweedie))

phi_tweedie <- round(summary(glm_tweedie)$dispersion,2)

# lm
lm <- lm(Score ~ Group, data=trial_data)
mu_lm <- coef(lm)[1]
mu_lm_treatment <- sum(coef(lm))
sigma_lm <- summary(lm)$sigma


#log-anova
delta <- 10.001
trial_data$log_score <- log(trial_data$Score + delta)
lm_log <- lm(log_score ~ Group, data=trial_data)
mu_lm_log <- coef(lm_log)[1]
mu_lm_log_treatment <- sum(coef(lm_log))
sigma_lm_log <- summary(lm_log)$sigma



# Creating a sequence for plotting
score_seq <- seq(0, max(trial_data$Score)+100, length.out = 100000)

# Theoretical densities
tweedie_density <- dtweedie(score_seq, mu = mu_tweedie, phi = phi_tweedie, power = var.power)
tweedie_density_treatment <- dtweedie(score_seq, mu = mu_tweeide_treatment, phi = phi_tweedie, power = var.power)

dtweedie(c(0,1,2,3), mu = mu_tweeide_treatment, phi = phi_tweedie, power = var.power)

lm_density <- dnorm(score_seq, mean = mu_lm, sd = sigma_lm)
lm_density_treatment <- dnorm(score_seq, mean = mu_lm_treatment, sd = sigma_lm)

log_density <- dnorm(log(score_seq+delta), mean = mu_lm_log, sd = sigma_lm_log)
log_density.back.tranfrom <- log_density*(1 / (score_seq + delta))
#https://www.cl.cam.ac.uk/teaching/0708/Probabilty/prob11.pdf
# the density transformation problem was why I used rnorm/rtweedie I think

log_density_treatment <- dnorm(log(score_seq+delta), mean = mu_lm_log_treatment, sd = sigma_lm_log)
log_density.back.tranfrom_treatment <- log_density_treatment*(1 / (score_seq + delta))


treatment_scores <- trial_data[trial_data$Group=="treatment",1]
control_scores <- trial_data[trial_data$Group=="control",1]


par(mfrow = c(1, 1))

# Plotting
hist(control_scores, breaks = 1000, probability = TRUE, col = "gray", border = "white",
     main = "Histogram of Scores and Densities",xlim=c(0,100), xlab = "Scores", ylab = "Density")
lines(score_seq, lm_density, col = "red", lwd = 2)
lines(score_seq, tweedie_density, col = "blue", lwd = 2)
lines(score_seq, log_density.back.tranfrom, col = "green", lwd = 2)
legend("topright", legend=c("Observed", "LM", "Tweedie" ,"Log Anova Back Transform"), col=c("black", "red", "blue","green"), lty=1, lwd=2)

hist(treatment_scores, breaks = 1000, probability = TRUE, col = "gray", border = "white",
     main = "Histogram of Scores and Densities", xlab = "Scores", ylab = "Density",xlim=c(0,100),ylim=c(0,0.07))
lines(score_seq, lm_density_treatment, col = "red", lwd = 2)
lines(score_seq, tweedie_density_treatment, col = "blue", lwd = 2)
lines(score_seq, log_density.back.tranfrom_treatment, col = "green", lwd = 2)
legend("topright", legend=c("Observed", "LM", "Tweedie" ,"Log Anova Back Transform"), col=c("black", "red", "blue","green"), lty=1, lwd=2)

# Zoomed plots

hist(control_scores, breaks = 1000, probability = TRUE, col = "gray", border = "white",
     main = "Histogram of Scores and Densities", xlab = "Scores", ylab = "Density", xlim=c(0,100))
lines(score_seq, lm_density, col = "red", lwd = 2)
lines(score_seq, tweedie_density, col = "blue", lwd = 2)
lines(score_seq, log_density.back.tranfrom, col = "green", lwd = 2)
legend("topright", legend=c("Observed", "LM", "Tweedie" ,"Log Anova Back Transform"), col=c("black", "red", "blue","green"), lty=1, lwd=2)

hist(treatment_scores, breaks = 300, probability = TRUE, col = "gray", border = "white",
     main = "Histogram of Scores and Densities", xlab = "Scores", ylab = "Density", xlim=c(0,100))
lines(score_seq, lm_density_treatment, col = "red", lwd = 2)
lines(score_seq, tweedie_density_treatment, col = "blue", lwd = 2)
lines(score_seq, log_density.back.tranfrom_treatment, col = "green", lwd = 2)
legend("topright", legend=c("Observed", "LM", "Tweedie" ,"Log Anova Back Transform"), col=c("black", "red", "blue","green"), lty=1, lwd=2)









if (!requireNamespace("transport", quietly = TRUE))
  install.packages("transport")
library(transport)

n_sim <- 1000000
# Simulate data from the fitted models
set.seed(123)  # For reproducibility

# Simulate from LM
sim_lm <- rnorm(n = n_sim, mean = mu_lm, sd = sigma_lm)

# Simulate from Tweedie
# Note: You might need to adjust the simulation method based on the availability and accuracy of simulating Tweedie distributions
sim_tweedie <- rtweedie(n = n_sim, mu = mu_tweedie, phi = phi_tweedie, power = var.power)

# Log-transform and back-transform for simulation
sim_log <- exp(rnorm(n = n_sim, mean = mu_lm_log, sd = sigma_lm_log)) - delta

# Empirical distribution of all_scores
empirical_scores <- all_scores

# Compute Wasserstein distances
wd_lm <- wasserstein1d(empirical_scores, sim_lm)
wd_tweedie <- wasserstein1d(empirical_scores, sim_tweedie)
wd_log <- wasserstein1d(empirical_scores, sim_log)

# Print results
print(paste("Wasserstein Distance LM:", wd_lm))
print(paste("Wasserstein Distance Tweedie:", wd_tweedie))
print(paste("Wasserstein Distance Log:", wd_log))




