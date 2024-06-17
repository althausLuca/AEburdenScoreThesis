library(quantreg)

run_qauntile_regression <- function(score_data, se="ker") {
  #https://cran.r-project.org/web/packages/quantreg/vignettes/rq.pdf
  quantile_regression_model <- rq(score_data$Score ~ score_data$Group)
  AIC <- AIC(quantile_regression_model)[1]
  # get the p-value for the treatment group coefficient and the standard error and t-value

  coefficient <- summary(quantile_regression_model)$coefficients[2, 1]
  lower_bound <- summary(quantile_regression_model)$coefficients[2, 2]
  upper_bound <- summary(quantile_regression_model)$coefficients[2, 3]

  ## add try catch block
  sid_quantille_summary <-  tryCatch({
    summary(quantile_regression_model, se = se)
  }, error = function(e) {
    NULL
  })

  if (is.null(sid_quantille_summary)) {
    p_value <- NA
    quantile_std_err <- NA
  }
  else {
    p_value <- sid_quantille_summary$coefficients[2, "Pr(>|t|)"]
    quantile_std_err <- sid_quantille_summary$coefficients[2, "Std. Error"]
  }

  results <- list("model" = QUANTILE_REGRESSION,
                  "AIC" = AIC,
                  "p_value" = p_value,
                  "estimate" = coefficient,
                  "lower_bound" = lower_bound,
                  "upper_bound" = upper_bound,
                  "std_err" = quantile_std_err)

  return(results)
}
