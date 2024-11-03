library(tweedie)
library(statmod)

#' Fit a tweedie model to the trial data (use mle is for the dispersion parameter)
fit_model.tweedie_glm_model <- function(model, trial,
                                        link_power = model$parameters$link_power,
                                        xi = model$parameters$xi,
                                        use_mle = model$parameters$use_mle) {
  trial <- check_data(trial)

  if (xi == "infer" || use_mle) {
    xi.vec <- if(xi == "infer") seq(1.05, 2, by = 0.1) else c(as.numeric(xi))
    profile_result <- tweedie.profile(trial$Score ~ trial$Group,
                                      link.power = link_power,
                                      fit.glm = F,
                                      xi.vec = xi.vec,
                                      method = "interpolation")
    if(xi == "infer"){
      xi <- profile_result$xi.max
    }
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

  # https://stats.stackexchange.com/questions/440227/glm-tweedie-dispersion-parameter
  summary_tweedie <- summary(tweedie_model, dispersion = phi)

  std_err <- summary_tweedie$coefficients[2, 2]

  #fixing dispersion by our own Estimation will compute the z-statistics
  t_value <- summary_tweedie$coefficients[2, 3]
  p_value <- 2 * pt(-abs(t_value),
                    df = tweedie_model$df.residual)#as I understood it we only subtract the lenght of \beta

  estimates <- list(
    mu_control = mu_control,
    mu_treatment = mu_treatment,
    phi = phi,
    xi = xi
  )

  #Correct for the estimated dispersion parameter by adding
  n_additonal_pars <-  (xi == "infer") + 1# 1 for phi as we set it ourselfs  and 1 for xi if it is infered
  AIC <- ifelse(xi != 0, AICtweedie(tweedie_model, dispersion = phi)+(2*n_additonal_pars), NULL)

  metrics <- list(
    AIC = AIC,
    p_value = p_value,
    std_err = std_err
  )

  results <- create_fitted_model_result(model, estimates, metrics)

  return(results)
}