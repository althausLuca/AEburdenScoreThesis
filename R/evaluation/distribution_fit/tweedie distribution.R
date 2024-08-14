

source("R/trials/trial_loader.R")
source("R/models/run_models.R")
source("../../models/model_CDFs.R")

trial_data <- load_longer_trials()


f <- 1/100

trial <- trial_data$trials[[5]]
trial$Score <- trial$Score*f
treatment_y <- trial$Score[trial$Group == "treatment"]
control_y <- trial$Score[trial$Group == "control"]

tweedie_model <- TWEEDIE_REGRESSION(var_power = 1.7 , link_power = 0)
model <- fit_model(tweedie_model, trial)

coefs <- split_model_coefficients(extract_coefficients(model))
coefs

treatment_coefs <- coefs$treatment
control_coefs <- coefs$control



x <- c(seq(-50,30,by=0.2),
         seq(31,50,by=0.5),
         seq(51,100, by=1),
         seq(100,600,by=5))*f

phi <- control_coefs$phi
treatment_distribution <- model_distribution(model = model, mu=treatment_coefs$mu , phi=phi , xi=treatment_coefs$xi , x)
control_distribution <- model_distribution(model = model, mu=control_coefs$mu , phi=phi , xi=control_coefs$xi , x)


par(mfrow=c(2,1))
plot(x,treatment_distribution)
# plot emprical distribution of treatment_y
lines(x, ecdf(treatment_y)(x))

plot(x,control_distribution)
# plot emprical distribution of control_y
lines(x, ecdf(control_y)(x))