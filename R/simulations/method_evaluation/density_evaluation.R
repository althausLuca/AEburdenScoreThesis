# Load the saved workspace
load("test.Rdata")

# Check if the 'results' data frame exists
if (exists("results")) {
  print("Results loaded successfully.")
  print(results)
} else {
  stop("No 'results' data frame found in the loaded workspace.")
}

# Load required functions
source("R/trials/trial_simulation.R")
source("../../models/density_methods.R")

# Load trial data
file <- file
trial_data <- get_trial_data(file)

group <- "control"

# Filter trials by group
trials <- lapply(trial_data$trials, function(trial) trial[trial$Group == group, ])

# Prepare to store densities and distributions
densities <- list()
distributions <- list()

# Common scale for plotting
x_values <- seq(0, max(unlist(lapply(trials, function(trial) trial$Score))) + 10, length.out = 1000)

# Loop through each trial to compute densities and distributions
for (trial_index in seq_along(trials)) {
  tweedie_xi <- NA
  tweedie_phi <- NA
  tweedie_mu <- NA

  if (group == "control") {
    tweedie_xi <- results$tweedie_xi[trial_index]
    tweedie_phi <- results$tweedie_phi[trial_index]
    tweedie_mu <- results$tweedie_mu[trial_index]
  } else if (group == "treatment") {
    tweedie_xi <- results$tweedie_xi[trial_index]
    tweedie_phi <- results$tweedie_phi[trial_index]
    tweedie_mu <- results$tweedie_mu_treatment[trial_index] + results$tweedie_mu[trial_index]
  }

  if (is.na(tweedie_xi)) {
    print("No more computed results available.")
    break
  }

  # Calculate the density of the Tweedie distribution
  density_values <- tweedie_density(trials[[trial_index]]$Score, mu = tweedie_mu, phi = tweedie_phi, power = tweedie_xi, x = x_values)
  densities[[trial_index]] <- density_values

  # Calculate the distribution (CDF) of the Tweedie distribution
  distribution_values <- tweedie_distribution(trials[[trial_index]]$Score, mu = tweedie_mu, phi = tweedie_phi, power = tweedie_xi, x = x_values)
  distributions[[trial_index]] <- distribution_values
}


color <- rgb(0, 0, 1, alpha = 0.04)
distributions_matrix <- do.call(cbind, distributions)

matplot(x_values, distributions_matrix, type = "l", lty = 1, col = color,
        xlim = c(-10, 200), ylim = c(0, 1),
        main = "",
        xlab = "Score", ylab = "P(x<=X)")

# Compute empirical CDF
all_scores <- trial_data$all_data()
all_scores <- all_scores[all_scores$Group == group, ]$Score
ecdf_func <- ecdf(all_scores)
ecdf_y_values <- ecdf_func(x_values)


lines(x_values, ecdf_y_values, col = "red", lwd = 2, lty = 1)
points(0,ecdf_y_values[2] ,col = "red" , cex=0.6, pch=16)
lines(c(-100,0),c(0,0), col = "red", lwd = 2, lty = 1)


# Add legend
legend("bottomright", legend = c("Tweedie Distributions", "Empirical CDF"),
       col = c(rgb(0, 0, 1, alpha = 0.8), "red"), lty = c(1, 1), lwd = c(1, 2), cex = 0.8, text.width=80)





matplot(x_values, distributions_matrix, type = "l", lty = 1, col = color,
        xlim = c(-2, 5), ylim = c(0, 1),
        main = "",
        xlab = "Score", ylab = "P(x<=X)")


lines(x_values, ecdf_y_values, col = "red", lwd = 2, lty = 1)
points(0,ecdf_y_values[2] ,col = "red" , cex=0.6, pch=16)
lines(c(-100,0),c(0,0), col = "red", lwd = 2, lty = 1)


# Add legend
legend("bottomright", legend = c("Tweedie Distributions", "Empirical CDF"),
       col = c(rgb(0, 0, 1, alpha = 0.8), "red"), lty = c(1, 1), lwd = c(1, 1), cex = 0.8, text.width=80)



# Compute and plot empirical density with point mass at zero
non_zero_scores <- all_scores[all_scores != 0]
density_non_zero <- density(non_zero_scores, from = 0, to = max(x_values), n = length(x_values))
proportion_zeros <- sum(all_scores == 0) / length(all_scores)
density_adjusted <- density_non_zero$y * (1 - proportion_zeros)

# library(ggplot2)
# library(ggbreak)
#
#
# density_data <- data.frame(
#   x = rep(x_values, length(densities)),
#   density = unlist(densities),
#   trial = rep(seq_along(densities), each = length(x_values))
# )
#
# empirical_density_data <- data.frame(
#   x = density_non_zero$x,
#   density = density_adjusted
# )
#
# g <- ggplot() +
#   geom_histogram(aes(x = all_scores, y = ..density..), breaks = seq(0, 200, length.out = 200), fill = "gray", color = "black", alpha = 0.5) +
#   geom_line(data = density_data, aes(x = x, y = density, group = trial), color = rgb(0, 0, 1, alpha = 0.07)) +
#   geom_line(data = empirical_density_data, aes(x = x, y = density), color = "green") +
#   geom_point(aes(x = 0, y = proportion_zeros), color = "green", size = 3) +
#   xlim(0, 200) +
#   labs(title = "Density plots", x = "Score", y = "Density")
#
# g
# g <- g + scale_y_break(c(0.025, proportion_zeros - 0.05) ,scales = 0.2) +
#     theme_bw() + theme(axis.text.x.top = element_blank(),
#                        axis.ticks.x.top = element_blank(),
#                        axis.line.x.top = element_blank())
# g
# ggsave("gg_cut.pdf", plot = g, width = 10, height = 5)
#
#
#
# g <- ggplot() +
#   geom_histogram(aes(x = all_scores, y = ..density..), breaks = seq(0, 200, length.out = 200), fill = "gray", color = "black", alpha = 0.5) +
#   geom_line(data = density_data, aes(x = x, y = density, group = trial), color = rgb(0, 0, 1, alpha = 0.07)) +
#   geom_line(data = empirical_density_data, aes(x = x, y = density), color = "green") +
#   geom_point(aes(x = 0, y = proportion_zeros), color = "green", size = 3) +
#   labs(title = "Density plots", x = "Score", y = "Density")
# print(g)
# g <- g +  xlim(0.0001, 200) + ylim(0,0.05)
# print(g)
color <- rgb(0, 0, 1, alpha = 0.1)

layout(matrix(c(2, 1), nrow = 1, byrow = TRUE), widths = c(0.8, 3))
par(mar = c(3, 2, 2, 3))

plot(c(100), c(0.05), xlim = c(0, 200), ylim = c(0,0.02), type = "n", main = "Density plots", xlab = "Score", ylab = "Density")

# Add histogram
hist(all_scores, breaks = c(-Inf,0.001, seq(1, max(x_values), by = 1)), probability = TRUE, col = rgb(0.9, 0.9, 0.9,1), border = rgb(0.9, 0.9, 0.9, 1), add = TRUE, xlim = c(0.1, 200), ylim = c(0, 0.1))

for (i in seq_along(densities)) {
  lines(x_values[-1], densities[[i]][-1], col = color)
}

lines(density_adjusted, col = "green", lwd = 2)


legend("topright", legend = c("Tweedie Sample Distributions","Hist of Scores", "Kernel Density of Non-Zero Scores"),
       col = c("blue", "gray", "green"), lty = c(1, 2, 1, 1), lwd = c(1, 2, 2, 2),
       cex = 0.8)


par(mar = c(3, 3, 2, 2))

zero_densities <- sapply(densities , function(x) x[1])

boxplot(zero_densities, horizontal = FALSE, col = "blue", pch = 50, outpch = 50, outcol = "blue", outcex = 0.5,
         xlab ="", main = "", ylim = c(0, max(zero_densities)))

abline(h=proportion_zeros, col = "black" , lw=4 , )
abline(h=proportion_zeros, col = "gray" , lw=3 , )
abline(h=mean(zero_densities), col = "red" , lw=3 , )


mtext("Zero Score Point Mass", side = 2, line = 2, cex = 1 , font = 2)


par(mar = c(2, 2, 2, 2))
layout(1)

