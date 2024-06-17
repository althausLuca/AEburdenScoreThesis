library(tweedie)
library(statmod)
#' Function to run Tweedie regression
#' Parameters:
#' - score_data: A data frame with two columns score and group
#' - link.power: link.power for the tweedy regression (default 0)
#' - var.power:  var.power for the tweedy regression (default 1.5)
#' Returns a list containing results from Tweedie regression
run_tweedie <- function(score_data, var.power = 1.5, link.power = 0) {
  tweedie_model <- glm(score_data$Score ~ score_data$Group, family =
    tweedie(var.power = var.power, link.power = link.power), control = glm.control(maxit = 100))

  AIC <- AICtweedie(tweedie_model, dispersion = 1)
  # get the p-value for the treatment group coefficient and the standard error and t-value
  summary_tweedy <- summary(tweedie_model)
  p_value <- summary_tweedy$coefficients[2, 4]
  std_err <- summary_tweedy$coefficients[2, 2]
  t_value <- summary_tweedy$coefficients[2, 3]
  estimate <- summary_tweedy$coefficients[2, 1]

  results <- list("model" = TWEEDIE,
                  "AIC" = AIC,
                  "p_value" = p_value,
                  "std_err" = std_err,
                  "t_value" = t_value,
                  "estimate" = estimate,
                  "link.power" = link.power,
                  "var.power" = var.power)

  return(results)
}
