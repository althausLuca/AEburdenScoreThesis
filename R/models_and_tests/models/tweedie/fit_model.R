library(tweedie)
library(statmod)

#' Fit a tweedie model to the trial data (use mle is for the dispersion parameter)
fit_model.tweedie_glm_model <- function(model, trial,
                                        link_power = model$parameters$link_power,
                                        xi = model$parameters$xi,
                                        use_mle = model$parameters$use_mle) {
  trial <- check_data(trial)
  initial_xi <- xi
  if (xi == "infer" || use_mle) {
    xi.vec <- ifelse(xi == "infer" ,seq(1.05, 2, by = 0.1) , c(as.numeric(xi)))
    profile_result <- tweedie.profile(trial$Score ~ trial$Group,
                                      link.power = link_power,
                                      fit.glm = F,
                                      xi.vec = xi.vec,
                                      method = "interpolation")
    xi <- profile_result$xi.max
    phi_mle <- profile_result$phi.max
  }

  xi <- as.numeric(xi)

  tweedie_model <- glm(trial$Score ~ trial$Group, family =
      statmod::tweedie(var.power = xi, link.power = link_power))

  phi_pearson <- sum(residuals(tweedie_model, type = "pearson")^2) / summary(tweedie_model)$df.residual

  phi <- ifelse(use_mle, phi_mle, phi_pearson)

  inverse_link_f <- tweedie_model$family$linkinv
  mu_control <- inverse_link_f(coef(tweedie_model)[1])
  mu_treatment <- inverse_link_f(sum(coef(tweedie_model)))

  summary_tweedie <- summary(tweedie_model, dispersion = phi)

  std_err <- summary_tweedie$coefficients[2, 2]
  #fixing dispersion by our own *Estimation* will compute the wrong statistics
  p_value <- 2 * pt(-abs(summary_tweedie$coefficients[2, 3]),
                    df = tweedie_model$df.residual)

  estimates <- list(
    mu_control = mu_control,
    mu_treatment = mu_treatment,
    phi = phi,
    xi = xi
  )

  #Correct for the estimated dispersion parameter by adding + 2
  AIC <- ifelse(xi != 0, AICtweedie(tweedie_model, dispersion = phi)+2, NULL)

  get_CDFs <- function(x) {
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
    initial_xi = initial_xi,
    get_CDFs = get_CDFs
  )

  return(results)
}