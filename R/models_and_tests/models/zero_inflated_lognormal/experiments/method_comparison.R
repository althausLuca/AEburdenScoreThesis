library(glmmTMB)
library(gamlss)
library(pscl)
library(Rfast2)


source("R/trials/trial_loader.R")

# trial_data <- load_equal_trials()
# trial <- trial_data$trials[[1]]

TMB_model <- glmmTMB(Score ~ Group, family = lognormal(link = "log"), data = trial, ziformula=~Group , dispformula = ~Group)
summary(TMB_model)
glmmTMB:::print.VarCorr.glmmTMB(TMB_model)
# Conditional model:
# Estimate Std. Error z value Pr(>|z|)
# (Intercept)     2.56845    0.16323  15.735   <2e-16 ***
# Grouptreatment  0.09591    0.12340   0.777    0.437
# Dispersion parameter for lognormal family (): 19.3
log(mean(scores_control.c))

mean <- fixef(TMB_model)[1]$cond[1]
meanlog <- fixef(TMB_model)[1]$cond[2]

scores_control.c <- trial$Score[trial$Group == "control" & trial$Score > 0]
scores_treatment.c <- trial$Score[trial$Group == "treatment" & trial$Score > 0]

fitdistr(scores_control.c, "lognormal")
# meanlog      sdlog
# 2.0507231   1.1533110
# (0.1846776) (0.1305868)




TMB_model <- glmmTMB(Score ~ Group, family = lognormal(link = "log"), data = trial, ziformula=~Group , dispformula = ~1)
summary(TMB_model)

n_sim <- 300

logit_inv <- function(x) {
  exp(x) / (1 + exp(x))
}


results <- data.frame(
  trial = numeric(),
  TMB_treatment_p_value = numeric(),
  TMB_bin_p_values = numeric(),
  gamlss_treatment_p_value = numeric(),
  gamlss_bin_p_values = numeric(),
  glm_treatment_p_value = numeric(),
  glm_bin_p_value = numeric()
)

for (i in seq_along(trial_data$trials)) {
  trial <- trial_data$trials[[i]]
  if (i > n_sim) { break }

  # model <- gamlss(Score ~ Group, sigma.formula = ~Group, nu.formula = ~Group,
  #                 family = ZAGA, data = trial)
  #
  # #p_value of mu for the treatment effect
  # inflated_p_value <- summary(model)[2, 4]

  mean(trial$Score > 0)
  control_mean <- mean(trial$Score[trial$Group == "control" & trial$Score > 0])
  treatment_mean <- mean(trial$Score[trial$Group == "treatment" & trial$Score > 0])
  control_mean
  treatment_mean

  ## TMB_mode
  TMB_model <- glmmTMB(Score ~ Group, family = ziGamma(link = "log"), data = trial, ziformula=~Group , dispformula = ~1)
  summary(TMB_model, robust = TRUE)$coefficients$cond
  summary(TMB_model, select = "fixed")
  class(TMB_model)

  TMB_treatment_p_value <- summary(TMB_model)$coefficients$cond[2, 4]
  TMB_bin_p_values <- summary(TMB_model)$coefficients$zi[2, 4]

  logit_inv(0.4473122)
  logit_inv(0.4473122 - 1.3426965)

  #galmss model
  gamlss_model <- gamlss(Score ~ Group, sigma.formula = ~1, nu.formula = ~Group, family = LOGNO(mu.link = "inverse"), data = trial)
  x <- summary(gamlss_model)
  gamlss_treatment_p_value <- summary(gamlss_model)[2, 4]
  gamlss_bin_p_values <- summary(gamlss_model)[5, 4]

  # glm model
  trial_non_zero <- trial[trial$Score > 0,]
  Z <- ifelse(trial$Score > 0, 1, 0)

  glm_model <- glm(Score ~ Group, family = Gamma(link = "log"), data = trial_non_zero)
  logistic_model <- glm(Z ~ trial$Group, family = binomial(link = "logit"))

  summary(logistic_model)
  summary(glm_model)

  exp(glm_model$coefficients[1])*(logit_inv(logistic_model$coefficients[1]))

  glm_treatment_p_value <- summary(glm_model)$coefficients[2, 4]
  glm_bin_p_value <- summary(logistic_model)$coefficients[2, 4]

  logLik(glm_model)
  # store results
    results <- rbind(results, data.frame(
        trial = i,
        TMB_treatment_p_value = TMB_treatment_p_value,
        TMB_bin_p_values = TMB_bin_p_values,
        gamlss_treatment_p_value = gamlss_treatment_p_value,
        gamlss_bin_p_values = gamlss_bin_p_values,
        glm_treatment_p_value = glm_treatment_p_value,
        glm_bin_p_value = glm_bin_p_value
    ))

}

results_sig <- results[,2:7] < 0.05
results_sig <- colSums(results_sig) / nrow(results)
results_sig

# vcov(gamlss_model)^0.5
# (Intercept) Grouptreatment  (Intercept)  (Intercept) Grouptreatment
# (Intercept)    1.191507e-01            NaN          NaN          NaN   4.030379e-06
# Grouptreatment          NaN   1.642379e-01          NaN 5.555494e-06            NaN
# (Intercept)             NaN            NaN 5.963925e-02          NaN   4.439088e-11
# (Intercept)             NaN   5.555494e-06          NaN 2.006431e-01            NaN
# Grouptreatment 4.030379e-06            NaN 4.439088e-11          NaN   2.862243e-01

estimates_conditional <- coef(summary(glm_model))[, "Estimate"]
std_errors_conditional <- coef(summary(glm_model))[, "Std. Error"]

z_values_conditional <- estimates_conditional / std_errors_conditional
p_values_conditional <- 2 * pnorm(-abs(z_values_conditional))



print(results)

zigamma.reg(trial$Score,trial$Group)$mod$phi


library(lmtest)

lrtest(glm_model)
lrtest(logistic_model)
lrtest(TMB_model)

summary(glm_model)
summary(TMB_model)

#split df data by group
control <- trial[trial$Group == "control" & trial$Score > 0,]
mean(control$Score)
treatment <- trial[trial$Group == "treatment" & trial$Score > 0,]
mean(treatment$Score)

TMB_model_gamma <- glmmTMB(Score ~ Group, family = Gamma(link = "log"), data = trial[trial$Score > 0,], dispformula = ~Group)
summary(TMB_model_gamma)$coefficients$cond
# summary(TMB_model_gamma)$coefficients$conddisp

TMB_model <- glmmTMB(Score ~ Group, family = ziGamma(link = "log"),data = trial, ziformula=~Group)
summary(TMB_model)$coefficients$cond



# glm_model <- glm(Score ~ Group, family = Gamma(link = "log"), data = trial[trial$Score > 0,])
# summary(glm_model)$coefficients
#
# vcov(TMB_model)





#AIC
# TMB_model.1 <- glmmTMB(Score ~ Group, family = ziGamma(link = "log"),data = trial, ziformula=~Group, dispformula = ~Group)
# TMB_model.2 <- glmmTMB(Score ~ Group, family = ziGamma(link = "log"),data = trial, ziformula=~Group , dispformula = ~1)
# AIC(TMB_model.1)
# AIC(TMB_model.2)


non_zero_trial <- trial[trial$Score > 0,]
ZAGA_model <- gamlss(Score ~ Group, sigma.formula = ~1, nu.formula = ~Group, family = ZAGA(mu.link = "log", sigma.link = "identity" ), data = trial)
GA_model <- gamlss(Score ~ Group, sigma.formula = ~ 1, family = GA(mu.link = "log" , sigma.link = "identity"), data = non_zero_trial , sigma.start = 1.139921 , sigma.fix = TRUE)
summary(GA_model, robust = TRUE)

ZAGA_model #   0.9321
GA_model #   0.9321   \sigma is the square root of the usual dispersion parameter for a GLM gamma

glm_model <- glm(Score ~ Group, family = Gamma(link = "log"), data = non_zero_trial)
summary(glm_model)$dispersion^0.5


exp( -0.07032)