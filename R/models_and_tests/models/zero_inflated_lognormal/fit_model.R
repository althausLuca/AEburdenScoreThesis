library(glmmTMB)
source("R/models_and_tests/models/zero_inflated_gamma/LRT_test.R")

fit_model.zero_inflated_lognormal <- function(model , trial, sigma_per_group = model$parameters$sigma_per_group) {
  # Ensure the data is properly formatted
  trial <- check_data(trial)

  # Fit the zero-inflated log-normal model using glmmTMB
  TMB_model <- tryCatch({
    if (sigma_per_group) {
      glmmTMB(Score ~ Group,
              ziformula = ~Group,
              dispformula = ~Group,
              family = lognormal(link = "log"),
              data = trial)
    } else {
      glmmTMB(Score ~ Group,
              ziformula = ~Group,
              dispformula = ~1,
              family = lognormal(link = "log"),
              data = trial)
    }
  }, error = function(e) {
    print(e$message)
    # Return an error classed object if the model fitting fails
    structure(list(message = e$message), class = c("glmmTMB_error", "model_error"))
  })

  # Extract coefficients
  fixed_effects <- fixef(TMB_model)
  mu_coefficients <- fixed_effects$cond
  nu_coefficients <- fixed_effects$zi
  sigma_coefficients <- fixef(TMB_model)$disp

  logit_inv <- function(x) { 1 / (1 + exp(-x)) }

  estimates <- list(
    mu_control = unname(exp(mu_coefficients[1])),
    mu_treatment = exp(sum(mu_coefficients)),
    nu_control = unname(logit_inv(nu_coefficients[1])),
    nu_treatment = logit_inv(sum(nu_coefficients))
  )

  if (sigma_per_group) {
    estimates$sigma_control <- unname(exp(sigma_coefficients[1]))
    estimates$sigma_treatment <- unname(exp(sum(sigma_coefficients)))
  } else {
    estimates$sigma <- unname(exp(sigma_coefficients[1]))
  }

  # Extract p-values from the model summary
  model_summary <- summary(TMB_model)
  mu_p_value <- model_summary$coefficients$cond[2, 4]
  nu_p_value <- model_summary$coefficients$zi[2, 4]
  AIC <- AIC(TMB_model)

  print("Running LRT test zero-inflated log-normal model")

  # LRT test fot the p_value
  p_value <- LRT_test(trial, dist = "lnorm", fix_arg = TRUE)

  # Define CDF functions for control and treatment groups
  get_CDFS <- function(x){

    # meanlog <- log(fixef(TMB_model)$cond^2/(fixef(TMB_model)$cond^2 + sigma(TMB_model)^2)^0.5)
    # sdlog <- log(1 + sigma(TMB_model)^2/(fixef(TMB_model)$cond^2))^0.5

    sigma_control <- ifelse(sigma_per_group, estimates$sigma_control, estimates$sigma)
    meanlog_control <- log(estimates$mu_control^2 / (estimates$mu_control^2 + sigma_control^2)^0.5)
    sdlog_control <- log(1 + sigma_control^2 / estimates$mu_control^2)^0.5

    sigma_treatment <- ifelse(sigma_per_group, estimates$sigma_treatment, estimates$sigma)
    meanlog_treatment <- log(estimates$mu_treatment^2 / (estimates$mu_treatment^2 + sigma_treatment^2)^0.5)
    sdlog_treatment <- log(1 + sigma_treatment^2 / estimates$mu_treatment^2)^0.5


    control_CDFs <- plnorm(x, meanlog = meanlog_control,
                           sdlog = sdlog_control) * (1 - estimates$nu_control) + estimates$nu_control*(x>=0)
    treatment_CDFs <- plnorm(x, meanlog = meanlog_treatment,
                             sdlog = sdlog_treatment) * (1 - estimates$nu_treatment) + estimates$nu_treatment*(x>=0)

    # control_CDFs <- plnorm(x, meanlog = log(estimates$mu_control),
    #                        sdlog = sigma_control) * (1 - estimates$nu_control) + estimates$nu_control
    # treatment_CDFs <- plnorm(x, meanlog = log(estimates$mu_treatment),
    #                          sdlog = sigma_treatment) * (1 - estimates$nu_treatment) + estimates$nu_treatment

    return(list(control = control_CDFs, treatment = treatment_CDFs))
  }

  # Return the results as a list
  result <- list(
    model = TMB_model,
    estimates = estimates,
    mu_p_val = mu_p_value,
    nu_p_val = nu_p_value,
    p_value = p_value,
    AIC = AIC,
    get_CDFs = get_CDFS
  )

  return(result)
}
