#' Functions to calculate the density and distribution fitted by the models (e.g Anova Model)
library(gamlss)
library(tweedie)
source("R/models/model_coefficients.R")


#helper function to split the coefficients and call the distribution function
get_model_distributions <- function(model, distribution_func, x) {
  splitted_coefs <- split_model_coefficients(extract_coefficients(model))

  splitted_coefs$control$x <- x
  splitted_coefs$treatment$x <- x

  result <- list()

  result$control <- do.call(distribution_func, splitted_coefs$control)
  result$treatment <- do.call(distribution_func, splitted_coefs$treatment)

  return(result)
}


# Define a generic function
model_distribution <- function(model, x, ...) {
  UseMethod("model_distribution")
}


### Define the distribution functions for each model
lm_distribution <- function(mu, sigma, x) {
  pnorm_results <- pnorm(x, mean = mu, sd = sigma)
  return(pnorm_results)
}

log_anova_distribution <- function(mu, sigma, c, x) {
  log_x <- ifelse(x + c > 0, log(x + c), -Inf)

  dnorm_results <- pnorm(log_x, mean = mu, sd = sigma)
  return(dnorm_results)
}

tweedie_distribution <- function(mu, phi, xi, x) {
  ptweedie_results <- ptweedie(x, mu = mu, phi = phi, power = xi)
  return(ptweedie_results)
}

zero_inflated_gamma_distribution <- function(mu, sigma, nu, x) {
  dgamma_results <- pZAGA(x, mu = mu, sigma = sigma, nu = nu)
  return(dgamma_results)
}

# zero_inflated_gamma_model.2 <- function(nu, shape, scale, x) {
#   result <- ifelse(x < 0, 0, ifelse(x == 0, nu, nu + (1 - nu) * pgamma(x, shape = shape, scale = scale)))
#   return(result)
# }


# Define the model_distribution functions for each model class
model_distribution.anova_ <- function(model, x) { get_model_distributions(model, lm_distribution, x) }

model_distribution.log_anova_ <- function(model, x) { get_model_distributions(model, log_anova_distribution, x) }

model_distribution.tweedie_glm_ <- function(model, x) { get_model_distributions(model, tweedie_distribution, x) }

model_distribution.gamlss <- function(model, x) { get_model_distributions(model, zero_inflated_gamma_distribution, x) }

model_distribution.zero_inflated_gamma_model <- function(model, x) { get_model_distributions(model, zero_inflated_gamma_model.2, x) }

model_distribution.default <- function(model, x) {
  print(paste0("Model: ", model$name, " with class ", class(model), " does not have a distribution function defined."))
  return(NULL)
}