source("R/methods/run_methods.R")

score_data <- data.frame(score = abs(c(rnorm(10, 0, 1), rnorm(10, 1, 1))),
                 group = c("control", "control", "control", "control", "control", "control", "control", "control", "control",
                           "control", "treatment", "treatment", "treatment", "treatment", "treatment", "treatment", "treatment",
                            "treatment", "treatment", "treatment"))
run_models(score_data)[[TWEEDIE]]$p_value
run_models(score_data)[[QUANTILE_REGRESSION]]$p_value

quantile_regression_model <- rq(score_data$score ~ score_data$group)
summary(quantile_regression_model, se = "nid")
AIC <- AIC(quantile_regression_model)[1]
# get the p-value for the treatment group coefficient and the standard error and t-value

coefficient <- summary(quantile_regression_model)$coefficients[2, 1]
lower_bound <- summary(quantile_regression_model)$coefficients[2, 2]
upper_bound <- summary(quantile_regression_model)$coefficients[2, 3]

sid_quantille_summary <- summary(quantile_regression_model, se = "nid") #or ker
p_value <- sid_quantille_summary$coefficients[2, "Pr(>|t|)"]
quantile_std_err <- sid_quantille_summary$coefficients[1, "Std. Error"]


