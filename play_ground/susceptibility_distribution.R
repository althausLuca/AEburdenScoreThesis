# Gamma distribution wieh mean 1 that can take large values
library(ggplot2)

n <- 100000
shape <- 0.5
scale <- 1/shape
x <- rgamma(n, shape, scale=scale)
mean(x)
mean(x)
max(x)
## ggplot2 histogram
ggplot(data.frame(x), aes(x)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.01, colour = "black", fill = "white") +
  stat_function(fun = dgamma, args = list(shape = shape, scale = scale), colour = "red", size = 1) +
  labs(title = "Gamma distribution with mean 1", x = "x", y = "Density") +
  theme_minimal()