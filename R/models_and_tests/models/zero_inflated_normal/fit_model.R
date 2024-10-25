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

  get_CDFs <- function(x) {
                #' Calculate the CDF of the zero-inflated normal distribution
                #' x - the value/values at which to calculate the CDF
                #' mu - the mean of the normal distribution of the non-zero values
                #' sigma - the standard deviation of the normal distribution of the non-zero values
                #' nu - the probability of obtaining a zero
    get_CDF <- function(x, mu, sigma, nu) {
      cdf <- nu * (x >= 0) + (1 - nu) * pnorm(x, mean = mu, sd = sigma)
      return(cdf)
    }

    res <- with(estimates, {
      treatment_CDF <- get_CDF(x, mu_treatment, ifelse(sigma_per_group, sigma_treatment, sigma), nu_treatment)
      control_CDF <- get_CDF(x, mu_control, ifelse(sigma_per_group, sigma_control, sigma), nu_control)

      return(list(control = control_CDF, treatment = treatment_CDF))
    })

    return(res)
  }


  ll_func <- function(x, mu, sigma, nu) {
    sum(ifelse(x == 0, log(nu), log(1 - nu) + dnorm(x, mean = mu, sd = sigma, log = TRUE)))
  }

  get_loglik <- function(full_trial = trial) {
    with(estimates, {
      ll <- ll_func(subset(full_trial$Score, full_trial$Group == "control"), mu_control,
                    ifelse(sigma_per_group, sigma_control, sigma), nu_control) +
        ll_func(subset(full_trial$Score, full_trial$Group == "treatment"), mu_treatment,
                ifelse(sigma_per_group, sigma_control, sigma), nu_treatment)
      return(ll)
    })
  }


  AIC <- -2 * get_loglik() + 2 * length(estimates)
  p_value <- NULL

  # Extract treatment effect p_value
  # if (!any(grepl("\\$get_CDFs", deparse(sys.calls())))) {
  #   p_value <- LRT_test(trial, dist = "norm", fix_arg = T)
  # } else { # skip computatio if not needed
  #
  # }

  result <- list(
    model = list(zero_model = zero_model),
    estimates = estimates,
    p_value = p_value,
    std_err = sigma,
    get_CDFs = get_CDFs,
    AIC = AIC
  )
}
