library(ggplot2)

#density_script_no_tweedie.Rdata
load("density_script_no_tweedie.Rdata", dt <- new.env())

delta <- dt$delta
file <- dt$file
results <- dt$results

rm(dt)

# Load required functions
source("R/trials/trial_simulation.R")
source("R/simulations/method_evaluation/density_methods.R")

#file.edit("R/simulations/method_evaluation/density_methods.R")

# Load trial data
trial_data <- get_trial_data(file)

group <- "treatment"

trials <- trial_data$trials
trials <- lapply(trial_data$trials, function(trial) trial[trial$Group == group,])

# Prepare to store distribution densities
distributions <- list()
densities <- list()

# Common scale for plotting
x_values <- c(seq(-delta,0, length.out = 100),
              seq(0, 50, length.out = 700) ,
              seq(50, max(unlist(lapply(trials, function(trial) trial$Score))) + 10, length.out = 300)
              )

for (trial_index in seq_along(trials)) {
  if(trial_index > 300){
    break
  }
  sigma <- results$lm_log_sigma[trial_index]
  if (group == "control") {
    mu <- results$lm_log_mu[trial_index]
  }

  if (group == "treatment") {
    mu <- results$lm_log_mu_treatment[trial_index] + results$lm_log_mu_[trial_index]
  }
  print(mu)
  if (is.na(sigma)) {
    print("No more computed results available.")
    break
  }
  # Calculate the density of the Tweedie distribution
  distribution_values <- log_anova_distribution(trials[[trial_index]]$Score, mu = mu, sigma = sigma, delta = delta, x = x_values)
  density_values <- log_anova_density(trials[[trial_index]]$Score, mu = mu, sigma = sigma, delta = delta, x = x_values)


  distributions[[trial_index]] <- distribution_values
  densities[[trial_index]] <- density_values
}

## whole plot -delta - 200
color <- rgb(0, 0, 1, alpha = 0.04)
color_legend <- rgb(0, 0, 1, alpha = 0.8)

distributions_matrix <- do.call(cbind, distributions)

# Use matplot to plot all densities at once
x_values_above_0 <- x_values[x_values>=0]
x_values_below_0 <- x_values[x_values<=0]

matplot(x_values, distributions_matrix, type = "l", lty = 1, col = color,
        xlim = c(-10, 600), ylim = c(0, 1),
        main = "",
        xlab = "Score", ylab = "P(x<=X)")


all_scores <- trial_data$all_data()
all_scores <- all_scores[all_scores$Group == group,]$Score
ecdf_func <- ecdf(all_scores)
ecdf_y_values <- ecdf_func(x_values)

zero_indices <- which(ecdf_y_values == 0)

lines(c(-100,x_values[zero_indices]), c(0,ecdf_y_values[zero_indices]), col = "red", lwd = 2, type= "l")
lines(x_values[-zero_indices], ecdf_y_values[-zero_indices], col = "red", lwd = 2, lty = 1)
points(0,ecdf_y_values[-zero_indices][1] ,col = "red" , cex=0.6, pch=16)

legend("bottomright", legend = c("Log-ANOVA Distributions", "Empirical CDF"),
       col = c(color_legend, "red"), lty = c(1, 1, 2), lwd = c(1, 2), cex =1.2 )


#zoomed in
matplot(x_values, distributions_matrix, type = "l", lty = 1, col = color,
        xlim = c(-delta-1, 5), ylim = c(0, 1),
        main = "",
        xlab = "Score", ylab = "P(x<=X)")


all_scores <- trial_data$all_data()
all_scores <- all_scores[all_scores$Group == group,]$Score
ecdf_func <- ecdf(all_scores)
ecdf_y_values <- ecdf_func(x_values)

zero_indices <- which(ecdf_y_values == 0)

lines(c(-delta-1,-delta), c(0,0), col = "blue", lwd = 2,lty = 1)

lines(c(-delta-1,-delta), c(0,0), col = "red", lwd = 2,lty = 2)
lines(c(-delta,0), c(0,0), col = "red", lwd = 2,lty = 1)

lines(x_values[-zero_indices], ecdf_y_values[-zero_indices], col = "red", lwd = 2,lty = 1)
points(0,ecdf_y_values[-zero_indices][1] ,col = "red" , cex=0.6, pch=16)
# Adding the custom tick at -delta
#axis(1, at = -delta, labels = paste0("-c"))

# Changing legend position to top right
legend("topleft", legend = c("Log-ANOVA Distributions", "Empirical CDF"),
       col = c(color_legend, "red"), lty = c(1, 1, 2), lwd = c(1, 2), cex = 1.2, text.width=250)

#Log_anova_full_l_control



# without 0 values
# paste0("Overlaid Log Anova Distributions for ", group, " trials (delta=", delta, ")")
matplot(x_values_above_0, distributions_matrix[x_values>=0,], type = "l", lty = 1, col = color,
        xlim = c(0, 600), ylim = c(0, 1),
        main = "",
        xlab = "Score", ylab = "P(x<=X)")

lines(x_values[-zero_indices], ecdf_y_values[-zero_indices], col = "red", lwd = 3, lty = 2)

legend("bottomright", legend = c("Log-ANOVA Distributions", "Empirical CDF"),
       col = c(color_legend, "red"), lty = c(1, 2, 2), lwd = c(1, 2), cex = 0.8)




non_zero_scores <- all_scores[all_scores != 0]
density_non_zero <- density(non_zero_scores, from = 0, to = max(x_values), n = length(x_values) , kernel = "gaussian")
# other kernels do not really improve this, the data is too sparse
#other approaches logsplines : https://stats.stackexchange.com/questions/65866/good-methods-for-density-plots-of-non-negative-variables-in-r

proportion_zeros <- sum(all_scores == 0) / length(all_scores)
density_adjusted <- density_non_zero$y * (1 - proportion_zeros)

color <- rgb(0, 0, 1, alpha = 0.03)


plot(c(100), c(0.05), xlim = c(0, 200), ylim = c(0, 0.1), type = "n", main = "Density plots", xlab = "Score", ylab = "Density")
# Add histogram
hist(all_scores, breaks = c(-Inf,0.001, seq(1, max(x_values), by = 1)), probability = TRUE, col = rgb(0.9, 0.9, 0.9,1), border = rgb(0.9, 0.9, 0.9, 1), add = TRUE, xlim = c(0.1, 200), ylim = c(0, 0.1))

density_matrix <- do.call(cbind, densities)

# Use matplot to plot all densities at once
matplot(x_values, density_matrix, type = "l", lty = 1, col = color , add=T)

lines(density_adjusted, col = "green", lwd = 2)
legend("topright", legend = c("Log Anova Sample Distributions","Hist of Scores", "Kernel Density of Non-Zero Scores"),
       col = c("blue", "gray", "green"), lty = c(1, 2, 1, 1), lwd = c(1, 2, 2, 2), cex = 0.8)


