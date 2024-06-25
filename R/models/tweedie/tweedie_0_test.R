library(statmod)
library(tweedie)

x <- seq(1,100,by=0.1)
mutrue <- exp(-1+x/25)

y <- rtweedie(length(x), mu=mutrue, phi=1, power=1.3)

sum(y==0)

fit <- glm(y ~ 1, family=tweedie(var.power=1.3, link.power=0))
round(summary(glm_tweedie)$dispersion, 2)

Phi <- 0.85786
summary(fit)
Mu <- coef(fit)[[1]]
Mu.inv <- fit$family$linkinv(Mu)

Power <- 1.3
Phi <- 1

Prob.Zero <- exp(-Mu^(2-Power) / Phi / (2-Power))
 exp(-Mu.inv^(2-Power) / Phi / (2-Power))

mean(y)

Prob.Zero
mean(y==0)


# Tweedie distribution
data <- c(rep(2,20),rep(0,20))
mean(data)
power  <- 1.2

fit <- glm(data ~ 1, family=tweedie(var.power=power, link.power=1))
Mu <- coef(fit)
mu <- exp(Mu)

mu <- 1
phi <- summary(fit)$dispersion



exp(-mu^(2-power) / phi / (2-power))
ptweedie(0, mu=mu, phi=phi, power=power)

