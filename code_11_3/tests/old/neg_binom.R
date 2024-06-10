library(MASS)

mean <-  60
size <- 1

prob <- size / (mean + size)

set.seed(123)  # for reproducibility
p_over_180 <-pnbinom(180, size = size, prob = prob, lower.tail = FALSE)
data <- rnbinom(n = 10000, size = size, prob = prob)
mean(data)
prob_of_300_or_more <- sum(data >= 300) / length(data)

print(prob_of_300_or_more)

# Visualize the distribution
hist(data, breaks = 50, main = paste0("NegBinomial mean=", mean ," P > 180 =",round(p_over_180,4)), xlab = "Value")
abline(v = 180, col = "red", lwd = 2)


mean <-  76
size <- 2

prob <- size / (mean + size)

set.seed(123)  # for reproducibility
p_over_180 <-pnbinom(180, size = size, prob = prob, lower.tail = FALSE)
data <- rnbinom(n = 10000, size = size, prob = prob)
mean(data)
prob_of_300_or_more <- sum(data >= 300) / length(data)

print(prob_of_300_or_more)

# Visualize the distribution
hist(data, breaks = 50, main = paste0("NegBinomial mean=", mean ," P > 180=",round(p_over_180,4)), xlab = "Value")
abline(v = 180, col = "red", lwd = 2)