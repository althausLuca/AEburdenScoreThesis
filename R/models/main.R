source("R/models/run_models.R")


test_data <- data.frame(
  Score = c(
    0, 0, 0, 0, 0, abs(rnorm(10, 5, 1)),
    0, 0, 0, abs(rnorm(12, 10, 1))
  ),
  Group = c(rep("control", 15), rep("treatment", 15))
)

run_log_anova(test_data, delta = 0.001)$p_value
run_anova(test_data)$p_value
run_tweedie(test_data, var.power = 1.65, link.power = 0)$p_value
run_qauntile_regression(test_data)$p_value



