library(statmod)
library(tweedie)

x <- seq(1,100,by=0.1)
mutrue <- exp(-1+x/25)

summary(mutrue)
y <- rtweedie(length(x), mu=mutrue, phi=1, power=1.3)
summary(y)

sum(y==0)

fit <- glm(y ~ 1, family=tweedie(var.power=1.3, link.power=0))



Phi <- 0.85786
e <- coef(fit)[[1]]

Power <- 1.3
Phi <- 1

Prob.Zero <- exp(-Mu^(2-Power) / Phi / (2-Power))

mean(y)

Prob.Zero

ptweedie(rep(0), mu=1, phi=2, power=Power)

mean(y==0)
