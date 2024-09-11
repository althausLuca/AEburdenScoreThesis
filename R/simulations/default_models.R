source("R/models_and_tests/models_and_tests.R")

DEFAULT_MODELS <- list(
  ANOVA(),
  LOG_ANOVA(c = 0.001),
  LOG_ANOVA(c = 1),
  LOG_ANOVA(c = 10000),
  TWEEDIE_REGRESSION(xi = 1.2),
  force_computation(QUANTILE_REGRESSION()),
  force_computation(QUANTILE_REGRESSION(tau = 0.75)),
  WILCOXON_TEST(),
  # PERMUTATION_TEST(),
  ZERO_INFLATED_GAMMA(),
  ZERO_INFLATED_LOGNORMAL(),
  TWO_PART_T_TEST(),
  TWO_PART_WILCOXON_TEST()
)

QR_MODELS <- list(
  force_computation(QUANTILE_REGRESSION(tau = 0.1)),
  force_computation(QUANTILE_REGRESSION(tau = 0.25)),
  force_computation(QUANTILE_REGRESSION()),
  force_computation(QUANTILE_REGRESSION(tau = 0.75)),
  force_computation(QUANTILE_REGRESSION(tau = 0.9)),
  force_computation(QUANTILE_REGRESSION(tau = 0.95))
)