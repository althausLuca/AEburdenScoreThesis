# Set up parameters
c <- 1
Y <- rpois(1000, 10)

# Log-transform the values
Y_log <- log(Y + c)

# Fit the linear model
lm_fit <- lm(Y_log ~ 1)

# Extract mean (mu) and standard deviation (sigma) from the model
mu <- summary(lm_fit)$coefficients[1]
sigma <- summary(lm_fit)$sigma

# Generate a sequence of log-transformed values
x_log <- seq(min(Y_log), max(Y_log), length.out = 1000)

# Generate the theoretical normal distribution on the log scale
theoretical_log_dist <- dnorm(x_log, mean = mu, sd = sigma)

# Back-transform the x values to the original scale
x_back <- exp(x_log) - c

# Back-transform the theoretical distribution
theoretical_back_dist <- theoretical_log_dist / (x_back + c)  # Adjust for the change in variable

# Plot the histogram of the original data
hist(Y, breaks = 30, probability = TRUE, main = "Histogram of Y with Back-Transformed Theoretical Distribution",
     xlab = "Y", col = "lightblue", border = "black")

# Add the back-transformed theoretical distribution to the plot
lines(x_back, theoretical_back_dist, col = "red", lwd = 2)

# Add a legend
legend("topright", legend = c("Histogram of Y", "Back-Transformed Theoretical Distribution"),
       col = c("black", "red"), lty = c(1, 1), lwd = c(1, 2))

# Plot the histogram of the log-transformed values
hist(Y_log, breaks = 30, probability = TRUE, main = "Histogram of Log(Y + c) with Theoretical Normal Distribution",
     xlab = "Log(Y + c)", col = "lightgreen", border = "black")

# Add the theoretical normal distribution on the log scale to the plot
lines(x_log, theoretical_log_dist, col = "blue", lwd = 2)

# Add a legend
legend("topright", legend = c("Histogram of Log(Y + c)", "Theoretical Normal Distribution"),
       col = c("black", "blue"), lty = c(1, 1), lwd = c(1, 2))


### CDF

# Generate the theoretical normal CDF on the log scale
theoretical_log_cdf <- pnorm(x_log, mean = mu, sd = sigma)

# Back-transform the x values to the original scale
x_back <- exp(x_log) - c

# Empirical CDF of the original data
ecdf_func <- ecdf(Y)

# Generate a sequence of values for the empirical CDF
x_empirical <- seq(min(Y), max(Y), length.out = 1000)
empirical_cdf <- ecdf_func(x_empirical)

# Plot the empirical CDF
plot(x_empirical, empirical_cdf, type = "l", col = "blue", lwd = 2,
     main = "Empirical CDF and Model CDF Comparison", xlab = "Y", ylab = "CDF")

# Add the back-transformed theoretical CDF to the plot
lines(x_back, theoretical_log_cdf, col = "red", lwd = 2, lty = 2)

# Add a legend
legend("bottomright", legend = c("Empirical CDF", "Back-Transformed Theoretical CDF"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)