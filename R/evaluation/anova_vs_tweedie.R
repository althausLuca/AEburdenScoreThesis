source("R/trials/trial_loader.R")
trial_data <- load_longer_trials()

trial.1 <- trial_data$trials[[1]]

anova_model <- fit_anova_model(trial.1)
tweedie_model <- fit_tweedie_model(trial.1 , link_power = 1)

summary(anova_model)
summary(tweedie_model)

#the way the variance-covariance matrix is calculated differs between models.


tweedie_model$residuals- anova_model$residuals

sum(tweedie_model$weights)


#The linear model assumes homoscedasticity (constant variance), leading to standard errors calculated based on this assumption.
#The Tweedie model allows for heteroscedasticity, leading to standard errors calculated based on the estimated variance function.

#https://statmath.wu.ac.at/courses/heather_turner/glmCourse_001.pdf

#glm use weights to calculate the variance-covariance matrix
#result only hold assymptotic properties


phi <- summary(tweedie_model)$dispersion
X <- model.matrix(tweedie_model)

sum(tweedie_model$weights)
W <- diag(tweedie_model$weights)
#variance-covariance matrix

vcov <- phi*solve(t(X) %*% W %*% X))

vcov^0.5
summary(tweedie_model)
