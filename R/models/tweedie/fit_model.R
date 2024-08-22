library(tweedie)
library(statmod)
source("R/models/model_settings.R")


fit_tweedie_model <- function(trial, link_power = 0, var_power = 1.5, xi = var_power) {
  intial_xi <- xi
  trial <- check_data(trial)
  if (xi == "infer") {
    print("infering xi using tweedie.profile()")
    profile_result <- tweedie.profile(trial$Score ~ trial$Group, link.power = link_power, fit.glm = F, xi.vec
      = seq(1.1, 2, by = 0.1))

    xi <- profile_result$xi.max

  }

  print(xi)
  print(link_power)
  print(trial$Score)
  tweedie_model <- glm(trial$Score ~ trial$Group, family =
    statmod::tweedie(var.power = xi, link.power = link_power))

  inverse_link_f <- tweedie_model$family$linkinv

  mu_control <- inverse_link_f(coef(tweedie_model)[1])
  mu_treatment <- inverse_link_f(sum(coef(tweedie_model)))

  phi <- round(summary(tweedie_model)$dispersion, 2)

  estimates <- list(
    mu_control = mu_control,
    mu_treatment = mu_treatment,
    phi = phi,
    xi = xi
  )


  AIC <- AICtweedie(tweedie_model, dispersion = phi)

  summary_tweedy <- summary(tweedie_model)
  p_value <- summary_tweedy$coefficients[2, 4]
  std_err <- summary_tweedy$coefficients[2, 2]

  get_CDFs <- function(x) {
    print(xi)
    treatment_CDF <- ptweedie(x, mu = mu_treatment, phi = phi, power = xi)
    control_CDF <- ptweedie(x, mu = mu_control, phi = phi, power = xi)
    return(list(control = treatment_CDF, treatment = control_CDF))
  }

  results <- list(
    model = tweedie_model,
    AIC = AIC,
    estimates = estimates,
    p_value = p_value,
    std_err = std_err,
    intial_xi = intial_xi,
    get_CDFs = get_CDFs
  )

  return(results)
}
