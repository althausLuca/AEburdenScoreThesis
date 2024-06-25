library(gamlss)


extract_gamlss_p_values <- function(gamlss_model) {
  model_summary <- summary(gamlss_model)

  mu_p_val <- model_summary[2, 4]
  sigma_p_val <- model_summary[4, 4]
  nu_p_pal <- model_summary[6, 4]

  p_values <- list("mu" = mu_p_val, "sigma" = sigma_p_val, "nu" = nu_p_pal)
  return(p_values)
}

