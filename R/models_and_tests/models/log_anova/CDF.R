log_anova_distribution <- function(mu, sigma, c, x) {
  log_x <- ifelse(x + c > 0, log(x + c), -Inf)
  dnorm_results <- pnorm(log_x, mean = mu, sd = sigma)
  return(dnorm_results)
}

