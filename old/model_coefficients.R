#' Function to split the model coefficients into control and treatment
#' @param model_coefficients The coefficients of the model
#' @return A list containing the control and treatment coefficients
#' @examples
#' model_coefficients <- list(nu_control = 1 , mu_treatment = 2 , phi = 3)
#' split_model_coefficients(model_coefficients)
#' result: list(control = c(nu=1,phi=3), treatment = c(mu=2,phi=3))
split_model_coefficients <- function(model_coefficients, include_common = TRUE) {
  coef_names <- names(model_coefficients)

  control_coef_names <- list()
  treatment_coef_names <- list()

  for (coef_name in coef_names) {
    if (grepl("_control", coef_name)) {
      control_coef_names[[sub('_control', '', coef_name)]] <- unname(model_coefficients[[coef_name]])
    }else if (grepl("_treatment", coef_name)) {
      treatment_coef_names[[sub('_treatment', '', coef_name)]] <- unname(model_coefficients[[coef_name]])
    }
    else if (include_common) {
      control_coef_names[[coef_name]] <- model_coefficients[[coef_name]]
      treatment_coef_names[[coef_name]] <- model_coefficients[[coef_name]]
    }
  }

  return(list(control = control_coef_names, treatment = treatment_coef_names))
}


# Define a generic function
extract_coefficients <- function(model, ...) {
  coefs <- extract_coefficients_(model, ...)
  return(lapply(coefs, function(x) unname(x)))
}

extract_coefficients_ <- function(model, ...) {
  UseMethod("extract_coefficients_")
}


#' Function to extract coefficients from a linear model
extract_coefficients_.log_anova_ <- function(model, ...) {
  lm_model <- model

  sigma <- summary(lm_model)$sigma

  mu_control <- coef(lm_model)[1]
  mu_treatment <- sum(coef(lm_model))

  return(list(mu_control = mu_control,
              mu_treatment = mu_treatment,
              sigma = sigma,
              c = model$c))
}


#' Function to extract coefficients from a Tweedie model
extract_coefficients_.glm <- function(model, ...) {
  if (model$family[[1]] == "Tweedie") {
    glm_tweedie <- model
    inverse_link_f <- glm_tweedie$family$linkinv

    mu_control <- inverse_link_f(coef(glm_tweedie)[1])
    mu_treatment <- inverse_link_f(sum(coef(glm_tweedie)))

    phi <- round(summary(glm_tweedie)$dispersion, 2)
    xi <- glm_tweedie$xi

    return(list(mu_control = mu_control,
                mu_treatment = mu_treatment,
                phi = phi,
                xi = xi))
  }
  stop("Only Tweedie family is supported")
}


extract_coefficients_.gamlss <- function(model, ...) {
  gamlss_model <- model
  mu_coefficients <- coef(gamlss_model, what = "mu")
  sigma_coefficients <- coef(gamlss_model, what = "sigma")
  nu_coefficients <- coef(gamlss_model, what = "nu")

  mu_control <- exp(mu_coefficients[1])
  mu_treatment <- exp(sum(mu_coefficients))

  sigma_control <- unname(exp(sigma_coefficients[1]))
  sigma_treatment <- exp(sum(sigma_coefficients))

  logit_inv <- function(x) { 1 / (1 + exp(-x)) }

  nu_control <- unname(logit_inv(nu_coefficients[1]))
  nu_treatment <- logit_inv(sum(nu_coefficients))

  return(list(mu_control = mu_control,
              mu_treatment = mu_treatment,
              sigma_control = sigma_control,
              sigma_treatment = sigma_treatment,
              nu_control = nu_control,
              nu_treatment = nu_treatment))
}

extract_coefficients_.quantile_regression_ <- function(model, ...) {
  rq_model <- model

  mu_control <- coef(rq_model)[1]
  mu_treatment <- sum(coef(rq_model))

  return(list(mu_control = mu_control,
              mu_treatment = mu_treatment))
}


extract_coefficients_.wilcoxon_test <- function(model, ...) {
  return(list())
}

extract_coefficients_.permutation_test <- function(model, ...) {
  return(list())
}


#
# extract_coefficients_.zero_inflated_gamma_model <- function(model, ...) {
#   logistic_glm <- model$logistic_glm
#   gamma_glm <- model$gamma_glm
#
#   mu_control <- exp(coef(gamma_glm)[1])
#   mu_treatment <- exp(coef(gamma_glm)[1] + coef(gamma_glm)[2])
#
#   phi <- summary(gamma_glm)$dispersion
#   shape <- 1 / phi
#   scale_control <- mu.mu_control / shape
#   scale_treatment <- mu_treatment / shape
#
#   nu_control <- coef(logistic_glm)[1]
#   nu_treatment <- sum(coef(logistic_glm))
#
#   #apply logit inverse
#   logit_inv <- function(x) { 1 / (1 + exp(-x)) }
#   nu_control <- unname(logit_inv(nu_control))
#   nu_treatment <- logit_inv(nu_treatment)
#
#   return(list(mu_control = mu_control,
#               mu_treatment = mu_treatment,
#               phi = phi,
#               shape = shape,
#               scale_control = scale_control,
#               scale_treatment = scale_treatment,
#               nu_control = nu_control,
#               nu_treatment = nu_treatment))
# }