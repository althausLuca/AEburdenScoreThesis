#' Functions to calculate the density and distribution fitted by the models (e.g Anova Model)
#' Y - The Values that were used as predictors in the model
library(gamlss)

lm_density <- function (Y, mu, sigma, x = seq(0, max(Y)+10, length.out = 100000)) {
  dnorm_results <- dnorm(x, mean = mu, sd = sigma)
  return(dnorm_results)
}

lm_distribution <- function (Y, mu, sigma, x = seq(0, max(Y)+10, length.out = 100000)) {
  pnorm_results <- pnorm(x, mean = mu, sd = sigma)
  return(pnorm_results)
}

library(tweedie)
tweedie_density <- function (Y, mu, phi, power, x = seq(0, max(Y)+10, length.out = 100000)) {
  dtweedie_results <- dtweedie(x, mu = mu, phi = phi, power = power)
  return(dtweedie_results)
}

tweedie_distribution <- function (Y, mu, phi, power, x = seq(0, max(Y)+10, length.out = 100000)) {
  ptweedie_results <- ptweedie(x, mu = mu, phi = phi, power = power)
  return(ptweedie_results)
}

log_anova_density <- function (Y, mu, sigma , c , x = seq(0, max(Y)+10, length.out = 100000)) {
  log_x <- ifelse( (x+c) > 0, log(x+c) , -Inf)
  dnorm_results <- dnorm(log_x, mean = mu, sd = sigma)
  # transofrm the variables back to the original scale
  dnorm_results <- dnorm_results*(1 / (x + c))
  return(dnorm_results)
}

log_anova_distribution <- function (Y, mu, sigma, c, x = seq(0, max(Y) + 10, length.out = 100000)) {
  # Compute log-transformed values
  log_x <- ifelse((x+c)> 0, log(x+c)  , -Inf)

  # Compute the density for the log-transformed values
  dnorm_results <- pnorm(log_x, mean = mu, sd = sigma)

  return(dnorm_results)
}

zero_inflated_gamma_distribution <- function (Y , mu, sigma, nu , x = seq(0, max(Y)+10, length.out = 100000)) {
  dgamma_results <-  pZAGA(x, mu = mu, sigma = sigma, nu = nu)
  return(dgamma_results)
}





# Define a generic function
model_distribution <- function(model, ...) {
  UseMethod("model_distribution")
}

model_distribution.anova_ <- function(model, mu , sigma , x) {
  return(lm_distribution(model$model$y, mu, sigma, x=x ))
}

model_distribution.log_anova_ <- function(model, mu , sigma , c , x ) {
  return(log_anova_distribution(Y=NULL, mu=mu, sigma=sigma, c = c , x=x ))
}

model_distribution.tweedie_glm_ <- function(model, mu , phi , xi , x ) {
  return(tweedie_distribution(model$model$y, mu, phi, xi, x=x ))
}

model_distribution.gamlss <- function(model, mu , sigma , nu , x ) {
  return(zero_inflated_gamma_distribution(model$model$y, mu, sigma, nu, x=x ))
}







