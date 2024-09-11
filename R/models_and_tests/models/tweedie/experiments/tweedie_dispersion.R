library(tweedie)
#https://stats.stackexchange.com/questions/224302/how-does-r-function-summary-glm-calculate-the-covariance-matrix-for-glm-model
source("R/trials/trial_loader.R")

trial_data <- load_longer_trials()

trial <- trial_data$trials[[2]]

model <- glm(trial$Score ~ trial$Group, family = statmod::tweedie(var.power = 1.4, link.power = 0))
summary(model)$dispersion


model <- glm(trial$Score ~ trial$Group, family = statmod::tweedie(var.power = 1.4, link.power = 1))
summary(model)$dispersion

model <- glm(trial$Score ~ trial$Group, family = statmod::tweedie(var.power = 1.4, link.power = 1))
summary(model)$dispersion

model.2 <- glm(trial$Score ~ trial$Group, family = statmod::tweedie(var.power = 1.7, link.power = 1))
summary(model.2)$dispersion

vcov(model)
vcov(model.2)

summary(model)
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)             7.872      1.456   5.406 1.84e-07 ***
# trial$Grouptreatment    9.133      2.890   3.160  0.00183 **

summary(model.2)
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)             7.872      1.368   5.755 3.26e-08 ***
# trial$Grouptreatment    9.133      2.967   3.079  0.00237 **


#dispersion parameter depends on power


t_value <- 9.133/vcov(model)[2,2]^0.5
t_value
p_value <- pt(t_value, df=200,   lower.tail = FALSE)

summary.glm(model)