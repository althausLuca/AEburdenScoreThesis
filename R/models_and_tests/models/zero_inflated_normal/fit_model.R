source("R/models_and_tests/tests/two_part_tests/two_part_methods.R")
source("R/models_and_tests/models/LRT_test.R")

fit_model.zero_inflated_normal <- function(model, trial) {
  trial <- check_data(trial)


  indices_c <- which(trial$Score != 0)
  normal_model <- lm(trial$Score[indices_c] ~ trial$Group[indices_c])

  # logistic regression model the probability of obtaining zero
  Z <- trial$Score
  Z[indices_c] <- 1

  zero_model <- glm(1 - Z ~ trial$Group, family = binomial(link = "logit"))

  nu_control <- zero_model$family$linkinv(coef(zero_model)[1])
  nu_treatment <- zero_model$family$linkinv(sum(coef(zero_model)))

  mu_treatment <- sum(coef(normal_model)[2])
  mu_control <- coef(normal_model)[1]

  sigma <- summary(normal_model)$sigma

  estimates <- list(
    mu_control = mu_control,
    mu_treatment = mu_treatment,
    sigma = sigma,
    nu_control = nu_control,
    nu_treatment = nu_treatment
  )

  # Extract treatment effect p_value
  if(!grepl("\\$get_CDFs", capture.output(print(sys.call())))[[1]]){
    p_value <- LRT_test(trial, dist = "norm")

  } else { # skip computatio if not needed
    p_value <- NULL
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

    treatment_CDF <- get_CDF(x, mu_treatment, sigma, nu_treatment)
    control_CDF <- get_CDF(x, mu_control, sigma, nu_control)

    return(list(control = control_CDF, treatment = treatment_CDF))
  }

  loglik <- numeric(length(trial$Score))
  loglik[trial$Group == "control" & trial$Score == 0] <- log(nu_control)
  control_cont <- trial$Group == "control" & trial$Score != 0
  loglik[control_cont] <- log(1 - nu_control) + dnorm(trial$Score[control_cont], mean = mu_control, sd = sigma, log = TRUE)
  treatment_cont <- trial$Group == "treatment" & trial$Score != 0
  loglik[treatment_cont] <- log(1 - nu_treatment) + dnorm(trial$Score[treatment_cont], mean = mu_treatment, sd = sigma, log = TRUE)
  log_likelihood <- sum(loglik)

  AIC <- -2 * log_likelihood + 2 * length(estimates)

  result <- list(
    model = list(zero_model=zero_model, normal_model=normal_model),
    estimates = estimates,
    p_value = p_value,
    std_err = sigma,
    get_CDFs = get_CDFs,
    c = c,
    AIC = AIC
  )
}


