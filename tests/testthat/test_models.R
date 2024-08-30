source("R/simulations/default_models.R")
library(testthat)

trial_1 <- data.frame(
  Score = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  Group = c(rep("control", 5), rep("treatment", 5))
)

trial_2 <- data.frame(
  Score = c(rep(0, 5), 6, 7, 8, 9, 10),
  Group = c(rep("control", 5), rep("treatment", 5))
)

trial_3 <- data.frame(
  Score = rep(0, 10),
  Group = c(rep("control", 5), rep("treatment", 5))
)

for (trial in list(trial_1, trial_2, trial_3)) {
  for (model in DEFAULT_MODELS) {
    if (inherits(model, "model")) {
      print(model$name)
      expect_no_error(fit_model(model, trial))
      # expect_gt(fitted <- fit_model(model, trial)$p_value, 0)
    }
    if (inherits(model, "test")) {
      print(model$name)
      expect_no_error(run_test(model, trial))
      expect_gt(run_test(model, trial)$p_value, 0)

    }
  }
}
