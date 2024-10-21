library(glmmTMB)
library(gamlss)
library(pscl)
library(Rfast2)


source("R/trials/trial_loader.R")

trial_data <- load_equal_trials()
trial <- trial_data$trials[[1]]

TMB_model <- glmmTMB(Score ~ Group, family = lognormal(link = "log"), data = trial, ziformula=~Group , dispformula = ~1)
summary(TMB_model)
AIC(TMB_model)
aic <- -2*logLik(TMB_model)[[1]]+2*length(TMB_model$fit$par)

TMB_model.2 <- glmmTMB(Score ~ Group, family = lognormal(link = "log"), data = trial, ziformula=~Group , dispformula = ~Group)
AIC(TMB_model.2)
aic.2 <- -2*logLik(TMB_model.2)[[1]]+2*length(TMB_model.2$fit$par)
aic.2


x <- logLik(TMB_model)
source("R/models_and_tests/models/LRT_test.R")
LRT_test(trial, dist = "lnorm", full_results = TRUE)
