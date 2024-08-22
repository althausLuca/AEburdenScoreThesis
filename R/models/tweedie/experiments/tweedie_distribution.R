library(tweedie)
library(ggplot2)
library(statmod)

n_samples = 1000
power = 2
mean = 1000
phi = 2


d_tweedie <- dtweedie(seq(1,100,0.01), xi=power, mu=mean , phi=phi)
plot(d_tweedie)
hist(d_tweedie , breaks = 50)

samples = rtweedie(n_samples, xi = power, mu = mean, phi = phi)
hist(samples, breaks = 50, main = "Histogram of Tweedie Distributed Data",
     xlab = "Values", ylab = "Frequency", col = "blue")

# Summary of the model
#summary(model)
#summary(model)$dispersion

#model = glm(samples ~ 1, family = tweedie(var.power = 1, link.power = 1))
#summary(model)
#fitted(model)

#mean(samples)


## duration scenario treatment
n_samples <- 10000
mean <- 307.75
phi <- 0.52
xi <- 1.65
samples = rtweedie(n_samples, xi = xi, mu = mean, phi = phi)
variance <- var(samples)
log_variance <- log(variance)
p1 <- ggplot(data.frame(score = samples), aes(x = score)) +
  geom_histogram(binwidth = 3, fill = "red", color = "black") +
  ggtitle(paste("Distribution of Control Scores drawn from rtweedie as in the duration scenario")) +
  xlim(0, 700) +
  annotate("text", x = Inf, y = Inf, label = paste(
    "\nVariance:", round(variance, 2),
    "\nLog Variance:", round(log_variance, 2)),
           hjust = 1.1, vjust = 1.1)
p1

## duration scenario control
n_samples <- 10000
mean <- 146.66
phi <- 0.52
xi <- 1.65
samples = rtweedie(n_samples, xi = xi, mu = mean, phi = phi)
variance <- var(samples)
log_variance <- log(variance)

p1 <- ggplot(data.frame(score = samples), aes(x = score)) +
  geom_histogram(binwidth = 3, fill = "red", color = "black") +
  ggtitle(paste("Distribution of Control Scores drawn from rtweedie as in the duration scenario")) +
  xlim(0, 700) +
  annotate("text", x = Inf, y = Inf, label = paste(
    "\nVariance:", round(variance, 2),
    "\nLog Variance:", round(log_variance, 2)),
           hjust = 1.1, vjust = 1.1)
p1

## severity scenario treatment
n_samples <- 10000
mean <- 66.2
phi <- 0.5
xi <- 1.8
samples = rtweedie(n_samples, xi = xi, mu = mean, phi = phi)
variance <- var(samples)
log_variance <- log(variance)

p2.1 <- ggplot(data.frame(score = samples), aes(x = score)) +
  geom_histogram(binwidth = 3, fill = "red", color = "black") +
  ggtitle(paste("Distribution of Control Scores drawn from rtweedie")) +
  xlim(0, 370) +
  annotate("text", x = Inf, y = Inf, label = paste(
    "\nVariance:", round(variance, 2),
    "\nLog Variance:", round(log_variance, 2)),
           hjust = 1.1, vjust = 1.1)
p2.1

## severity scenario
n_samples <- 10000
mean <- 100
phi <- 0.5
xi <- 1.8
samples = rtweedie(n_samples, xi = xi, mu = mean, phi = phi)

p2.2 <- ggplot(data.frame(score = samples), aes(x = score)) +
  geom_histogram(binwidth = 3, fill = "red", color = "black") +
  ggtitle(paste("Distribution of Control Scores drawn from rtweedie")) +
  xlim(0, 370) +
  annotate("text", x = Inf, y = Inf, label = paste(
    "\nVariance:", round(variance, 2),
    "\nLog Variance:", round(log_variance, 2)),
           hjust = 1.1, vjust = 1.1)
p2.2
