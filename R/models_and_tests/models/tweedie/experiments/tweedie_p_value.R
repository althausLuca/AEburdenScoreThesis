library(tweedie)
#https://stats.stackexchange.com/questions/224302/how-does-r-function-summary-glm-calculate-the-covariance-matrix-for-glm-model
source("R/trials/trial_loader.R")

trial_data <- load_longer_trials()

trial <- trial_data$trials[[2]]

model <- glm(trial$Score ~ trial$Group, family = statmod::tweedie(var.power = 1.4, link.power = 0))
model$weights
summary(model)

X <- model.matrix(model)
qr.solve(t(X) %*% diag(model$weights) %*% X) * summary(model)$dispersion
vcov(model)

vcov(model)/summary(model)$cov.unscaled

summary(model)$dispersion

var(model$residuals)
mean(model$residuals)


model <- glm(trial$Score ~ trial$Group, family =  poisson(link = "log"))
X <- model.matrix(model)
qr.solve(t(X) %*% diag(model$weights) %*% X) * summary(model)$dispersion
vcov(model)

model <- glm(trial$Score ~ trial$Group, family =  gaussian(link = "identity"))
summary(model)$dispersion
qr.solve(t(X) %*% diag(model$weights) %*% X) * summary(model)$dispersion
vcov(model)

model <- glm(trial$Score ~ trial$Group, family =  Gamma(link = "inverse"))
summary(model)$dispersion
qr.solve(t(X) %*% diag(model$weights) %*% X) * summary(model)$dispersion
vcov(model)


info(model)

