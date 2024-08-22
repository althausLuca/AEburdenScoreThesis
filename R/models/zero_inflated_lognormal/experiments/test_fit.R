source("R/trials/trial_loader.R")
source("R/models/zero_inflated_lognormal/fit_model.R")

trial_data <- load_longer_trials()

trial <- trial_data$trials[[1]]


fit <- fit_zero_inflated_lognormal_model(trial)

fit$p_value
fit$estimates
fit$mu_p_val
fit$nu_p_val



library(fitdistrplus)
Scores <- trial$Score
Scores.c <- Scores[Scores > 0]

fitted_dist <- fitdist(Scores[Scores > 0], "lnorm", calcvcov = FALSE, keepdata = FALSE)

fitted_dist
log(sigma(fit$model)^0.5)
log(sigma(fit$model))^0.5

fixef(fit$model)$cond

TMB_model <- glmmTMB(Score ~ 1, family = lognormal(link = "identity"), data = trial, ziformula=~1 , dispformula = ~1)
fitted_dist <- fitdist(Scores[Scores > 0], "lnorm")

exp(fitted_dist$estimate[1] + 1/2*fitted_dist$estimate[2]^2 )
fixef(TMB_model)$cond


log(fixef(TMB_model)$cond)

TMB_model$modelInfo$family


meanlog <- log(fixef(TMB_model)$cond^2/(fixef(TMB_model)$cond^2 + sigma(TMB_model)^2)^0.5)
sdlog <- log(1 + sigma(TMB_model)^2/(fixef(TMB_model)$cond^2))^0.5