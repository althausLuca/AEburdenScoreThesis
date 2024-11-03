source("R/models_and_tests/tests/two_part_tests/two_part_methods.R")
source("R/models_and_tests/models/LRT_test.R")

zero_model <- function(trial) {
  Z <- (trial$Score == 0) * 1
  zero_model <- glm(Z ~ trial$Group, family = binomial(link = "logit"))
  nu_control <- unname(zero_model$family$linkinv(coef(zero_model)[1]))
  nu_treatment <- zero_model$family$linkinv(sum(coef(zero_model)))
  return(list(model = zero_model, estimates = list(nu_control = nu_control, nu_treatment = nu_treatment)))
}

fit_model.zero_inflated_normal <- function(model, trial, sigma_per_group = model$parameters$sigma_per_group) {
  trial <- check_data(trial)

  indices_c <- which(trial$Score != 0)
  trial_c <- trial[indices_c,]

  estimates <- zero_model(trial)$estimates

  estimates$mu_control <- mean(trial_c$Score[trial_c$Group == "control"])
  estimates$mu_treatment <- mean(trial_c$Score[trial_c$Group == "treatment"])

  if (sigma_per_group) {
    control_scores_c <- trial_c$Score[trial_c$Group == "control"]
    treatment_scores_c <- trial_c$Score[trial_c$Group == "treatment"]
    estimates$sigma_control <- sd(control_scores_c)
    estimates$sigma_treatment <- sd(treatment_scores_c)
  } else {
    estimates$sigma <- summary(lm(Score ~ Group, data = trial_c))$sigma
  }


  ll_func <- function(x, mu, sigma, nu) {
    sum(ifelse(x == 0, log(nu), log(1 - nu) + dnorm(x, mean = mu, sd = sigma, log = TRUE)))
  }

  sigma_control <- ifelse(sigma_per_group, estimates$sigma_control, estimates$sigma)
  sigma_treatment <- ifelse(sigma_per_group, estimates$sigma_treatment, estimates$sigma)

  ll <- ll_func(subset(trial$Score, trial$Group == "control"), estimates$mu_control, sigma_control, estimates$nu_control)
  ll <- ll + ll_func(subset(trial$Score, trial$Group == "treatment"), estimates$mu_treatment, sigma_treatment, estimates$nu_treatment)

  AIC <- -2 * ll + 2 * length(estimates)
  p_value <- NULL #we dont use this in the model

  metrics <- list(
    AIC = AIC,
    p_value = p_value
  )

  result <- create_fitted_model_result(model, estimates, metrics)
}
#
# head(model_computer$
#        model_estimates$
#        zero_inflated_normal_per_group)
#
# m_control <- max(unlist(sigma_c <- model_computer$
#   model_estimates$
#   zero_inflated_normal_per_group[, 5]))
# m_treatment <- max(unlist(sigma_t <- model_computer$
#   model_estimates$
#   zero_inflated_normal_per_group[, 6]))
#
# head(model_computer$
#        model_metrics$
#        zero_inflated_normal_per_group)
# aic_values <- model_computer$
#   model_metrics$
#   zero_inflated_normal_per_group[, 1]
# m_aic <- max(unlist(aic_values))
# i <- which(aic_values == m_aic)
#
# sigma_c[[i]]
# sigma_t[[i]]
#
# model_computer$
#   model_metrics$
#   zero_inflated_normal_per_group[i, 1]
# estimates <- model_computer$
#   model_estimates$
#   zero_inflated_normal_per_group[i,]
# trial <- model_computer$trial_data$trials[[i]]
#
# ll_func <- function(x, mu, sigma, nu) {
#   sum(ifelse(x == 0, log(nu), log(1 - nu) + dnorm(x, mean = mu, sd = sigma, log = TRUE)))
# }
#
# x_control <- trial[trial$Group == "control",]$Score
# x_treatment <- trial[trial$Group == "treatment",]$Score
#
# ll <- ll_func(x_control, estimates$mu_control, estimates$sigma_control, estimates$nu_control) +
#   ll_func(x_treatment, estimates$mu_treatment, estimates$sigma_treatment, estimates$nu_treatment)
# -2 * ll + 2 * length(estimates)
#
# fit_model(model_computer$
#             models$
#             zero_inflated_normal_per_group, trial)$metrics$AIC
