
get_suscebilities <- function(n, type = "gamma", param = 2) {
  if(type  == "constant") {
    susceptibility <- rep(param, n)
  } else if (type == "binary") { #second argument is the probability of not being susceptible
    unif_rvs <- runif(n, min = 0, max = 1)
    susceptibility <- (unif_rvs > param) * 1
    # normalize to expected value of 1
    susceptibility <- susceptibility / (1 - as.double(susceptibility_parameter[2]))
  } else {
    shape <- param
    scale <- 1 / shape
    susceptibility <- rgamma(n, shape, scale = scale)
  }
  return(susceptibility)
}