#' Functions to calculate the density and distribution fitted by the models (e.g Anova Model)
#' Y - The Values that were used as predictors in the model

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

log_anova_density <- function (Y, mu, sigma , delta , x = seq(0, max(Y)+10, length.out = 100000)) {
  log_x <- log(x+delta)
  dnorm_results <- dnorm(log_x, mean = mu, sd = sigma)
  # transofrm the variables back to the original scale
  dnorm_results <- dnorm_results*(1 / (x + delta))
  return(dnorm_results)
}

log_anova_distribution <- function (Y, mu, sigma, delta, x = seq(0, max(Y) + 10, length.out = 100000)) {
  # Compute log-transformed values
  log_x <- log(x + delta)

  # Compute the density for the log-transformed values
  dnorm_results <- pnorm(log_x, mean = mu, sd = sigma)

  return(dnorm_results)
}
