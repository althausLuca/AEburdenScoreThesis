

x <- c(8.50, 9.48, 8.65, 8.16, 8.83, 7.76, 8.63)
y <- c(8.27 ,8.20 ,8.25, 8.14, 9.00, 8.10, 7.20, 8.32, 7.70)

wt <- wilcox.test(x, y, alternative = "two.sided")
wt$statistic
wt$p.value

x_rank_sum <- rank(c(x,y))[1:length(x)]
y_rank_sum <- rank(c(x,y))[(length(x)+1):(length(y)+length(x))]

R_x <- sum(x_rank_sum)
R_y <- sum(y_rank_sum)


U_x <-  length(x)*(length(x)+1)/2 + length(x)*length(y) -R_x
U_y <-  length(y)*(length(y)+1)/2 + length(x)*length(y) -R_y

my_U <- length(x)*length(y)/2
my_sd <- sqrt(length(x)*length(y)*(length(x)+length(y)+1)/12)


p_value <- pnorm(U_y, mean = my_U, sd = my_sd, lower.tail = FALSE)
p_value <- pnorm(U_x, mean = my_U, sd = my_sd, lower.tail = FALSE)


## corrected statistics
p_value <- pchisq(75, df = 1, lower.tail = FALSE)

W <- wt$statistic
U1 <- W - (n_x * (n_x + 1)) / 2
U2 <- n_x * n_y - U1
U <- min(U1, U2)



# Example chi-square statistic
chi_square_stat <- 75

# Degrees of freedom
df <- 1

# Calculate the p-value
p_value <- pchisq(chi_square_stat, df = df, lower.tail = FALSE)

sum(x_rank_sum)/length(x) - sum(y_rank_sum)*length(y)


w_a <- 75  # Example test statistic
df <- 1  # Degrees of freedom for a two-sample Wilcoxon rank-sum test

# Calculate the p-value
p_value <- pchisq(w_a, df = df, lower.tail = FALSE)
print(p_value)

corrected_stat <- w_a - 0.5 * (length(x) * length(y))

# Calculate the corrected p-value
p_value_corrected <- pchisq(corrected_stat, df = df, lower.tail = FALSE)
print(p_value_corrected)

W <- wt$statistic
n_x <- length(x)
n_y <- length(y)
U <- W - (n_x * (n_x + 1)) / 2
U_corrected <- U + 0.5 * (n_x * n_y)
p_value <- pnorm(U_corrected, mean = n_x * n_y / 2,
                 sd = sqrt(n_x * n_y * (n_x + n_y + 1) / 12), lower.tail = FALSE)

p_value


# Given data
x <- c(8.50, 9.48, 8.65, 8.16, 8.83, 7.76, 8.63)
y <- c(8.27 ,8.20 ,8.25, 8.14, 9.00, 8.10, 7.20, 8.32, 7.70)

# Given rank sum for x
R_x <- 75
n_x <- length(x)
n_y <- length(y)

# Calculate U statistic for x
U_x <- R_x - (n_x * (n_x + 1)) / 2

# U statistic for y (alternative way)
U_y <- n_x * n_y - U_x

# Calculate the p-value using the U statistic
# Use the normal approximation if the sample size is large enough
mean_U <- n_x * n_y / 2
sd_U <- sqrt(n_x * n_y * (n_x + n_y + 1) / 12)

# Calculate the z value for the normal distribution
z <- (U_x - mean_U) / sd_U

# Two-tailed p-value
p_value <- 2 * pnorm(abs(z), lower.tail = FALSE)
print(p_value)