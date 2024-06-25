#' Inspect how the power parameter \xi and dispersion parameter \phi differ between
#' the full model and the individual groups

source("../../models/density_methods.R")


source("R/trials/trial_simulation.R")

link_power <- 1
file <- "Scenario_2_k_1.5_s_0.5.csv"

trial_data <- get_trial_data(file)

trial <- trial_data$trials[[5]] # get one trial

trial$Score <- trial$Score*1

control <- trial[trial$Group=="control",]
treatment <- trial[trial$Group=="treatment",]


##full trial

profile_result <- tweedie.profile(trial$Score ~ trial$Group, link.power = link_power, fit.glm=F , xi.vec
                                  = seq(1.1, 2, by=0.1))

optimal_xi_full <- profile_result$xi.max
var.power_full <- optimal_xi_full

glm_tweedie <- glm(Score ~ Group, data=trial, family = tweedie(var.power = optimal_xi_full, link.power = 1))

mu_tweedie_full <- coef(glm_tweedie)[1]
mu_tweedie_full_treatment <-  sum(coef(glm_tweedie))
phi_tweedie_full <- round(summary(glm_tweedie)$dispersion,2)


# Control group only
profile_result_control <- tweedie.profile(control$Score ~ 1, link.power = link_power, fit.glm = FALSE, xi.vec = seq(1.1, 2, by = 0.1))

optimal_xi_control <- profile_result_control$xi.max
var.power_control <- optimal_xi_control

glm_tweedie_control <- glm(Score ~ 1, data = control, family = tweedie(var.power = var.power_control, link.power = 1))

mu_tweedie_control <- coef(glm_tweedie_control)[1]
phi_tweedie_control <- round(summary(glm_tweedie_control)$dispersion, 2)



# Treatment group only
profile_result_treatment <- tweedie.profile(treatment$Score ~ 1, link.power = link_power, fit.glm = FALSE, xi.vec = seq(1.1, 2, by = 0.1))

optimal_xi_treatment <- profile_result_treatment$xi.max
var.power_treatment <- optimal_xi_treatment

glm_tweedie_treatment <- glm(Score ~ 1, data = treatment, family = tweedie(var.power = var.power_treatment, link.power = 1))

mu_tweedie_treatment <- coef(glm_tweedie_treatment)[1]
phi_tweedie_treatment <- round(summary(glm_tweedie_treatment)$dispersion, 2)

# Print results
cat("Full trial model:\n")
cat("Optimal xi:", optimal_xi_full, "\n")
cat("Mu:", mu_tweedie_full, "\n")
cat("Mu (treatment):", mu_tweedie_full_treatment, "\n")
cat("Phi:", phi_tweedie_full, "\n\n")

cat("Control group model:\n")
cat("Optimal xi:", optimal_xi_control, "\n")
cat("Mu:", mu_tweedie_control, "\n")
cat("Phi:", phi_tweedie_control, "\n\n")

cat("Treatment group model:\n")
cat("Optimal xi:", optimal_xi_treatment, "\n")
cat("Mu:", mu_tweedie_treatment, "\n")
cat("Phi:", phi_tweedie_treatment, "\n")




# Generate x-values for density and ECDF
x_values <- seq(-100, 600, by=0.01)

# Calculate CDF values
cdf_values_full_control <- tweedie_distribution(NULL, x = x_values, mu = mu_tweedie_full, phi = phi_tweedie_full, power = optimal_xi_full)
cdf_values_full_treatment <- tweedie_distribution(NULL, x = x_values, mu = mu_tweedie_full_treatment, phi = phi_tweedie_full, power = optimal_xi_full)
cdf_values_control <- tweedie_distribution(NULL, x = x_values, mu = mu_tweedie_control, phi = phi_tweedie_control, power = optimal_xi_control)
cdf_values_treatment <- tweedie_distribution(NULL, x = x_values, mu = mu_tweedie_treatment, phi = phi_tweedie_treatment, power = optimal_xi_treatment)

# Calculate ECDF values
ecdf_control <- ecdf(control$Score)
ecdf_treatment <- ecdf(treatment$Score)

# Plot for Control Group
par(mfrow = c(2, 1))  # Set layout to have 2 plots, one above the other

plot(ecdf_control, main = "Control Group emprical CDF and Fitted CDFs", xlab = "Score", ylab = "CDF", col = "blue", verticals = TRUE, do.points = FALSE)
lines(x_values, cdf_values_control, col = "red", lty = 1)
lines(x_values, cdf_values_full_control, col = "darkgreen", lty = 2)
legend("bottomright", legend = c(
  paste("Empricial "),
  paste("Control only: mu =", round(mu_tweedie_control, 2), ", phi =", phi_tweedie_control, ", xi =", round(optimal_xi_control, 2)),
  paste("Combined: mu =", round(mu_tweedie_full, 2), ", phi =", phi_tweedie_full, ", xi =", round(optimal_xi_full, 2))
), col = c("blue", "red", "darkgreen"), lty = c(1, 1, 2))

# Plot for Treatment Group
plot(ecdf_treatment, main = "Treatment Group empricical CDF and Fitted CDFs", xlab = "Score", ylab = "CDF", col = "blue", verticals = TRUE, do.points = FALSE)
lines(x_values, cdf_values_treatment, col = "red", lty = 1)
lines(x_values, cdf_values_full_treatment, col = "darkgreen", lty = 2)
legend("bottomright", legend = c(
  paste("Empricial"),
  paste("Treatment only: mu =", round(mu_tweedie_treatment, 2), ", phi =", phi_tweedie_treatment, ", xi =", round(optimal_xi_treatment, 2)),
  paste("Combined: mu =", round(mu_tweedie_full_treatment, 2), ", phi =", phi_tweedie_full, ", xi =", round(optimal_xi_full, 2))
), col = c("blue", "red", "darkgreen"), lty = c(1, 1, 2))
