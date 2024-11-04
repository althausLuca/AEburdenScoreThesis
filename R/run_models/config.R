#' settings for the results generation
#' By default the results of the models will be stored in MODEL_RESULT_PATH
#' The trial data is expected to be in TRIAL_DATA_PATH
#' in the same structure as the folder conting the trial data e.g. data/trials
source("R/models_and_tests/models_and_tests.R")
source("R/run_models/model_computer.R")
source("R/data_generation/config_and_init.R", local = (dg_config <- new.env()))

CLEAR_ALL <- FALSE # recompute everything
HALT_ON_ERROR <- TRUE #  stop or compute what works

TRIAL_DATA_PATH <-  Sys.getenv("TRIAL_FOLDER", unset = "data/trials/")
MODEL_RESULT_PATH <- Sys.getenv("RESULT_FOLDER", unset = "results/")

DEFAULT_MODELS <- list(
  ANOVA(),
  LOG_ANOVA(c = 0.00001),
  LOG_ANOVA(c = 0.001),
  LOG_ANOVA(c = 1),
  LOG_ANOVA(c = 10000),
  TWEEDIE_REGRESSION(xi = 1.2),
  TWEEDIE_REGRESSION(xi = 1.8),
  TWEEDIE_REGRESSION(xi = "infer"),
  TWEEDIE_REGRESSION(xi = "infer", use_mle = T),
  TWEEDIE_REGRESSION(xi = 1.2, use_mle = T),
  TWEEDIE_REGRESSION(xi = 1.8, use_mle = T),
  QUANTILE_REGRESSION(),
  QUANTILE_REGRESSION(tau = 0.75),
  WILCOXON_TEST(),
  PERMUTATION_TEST(),
  ZERO_INFLATED_GAMMA(),
  ZERO_INFLATED_LOGNORMAL(),
  TWO_PART_T_TEST(),
  TWO_PART_WILCOXON_TEST(),
  TWO_PART_T_TEST(use_welch = TRUE),
  ZERO_INFLATED_LOGNORMAL(sigma_per_group = TRUE),
  ZERO_INFLATED_GAMMA(sigma_per_group = TRUE),
  QUANTILE_REGRESSION(tau = 0.05),
  QUANTILE_REGRESSION(tau = 0.1),
  QUANTILE_REGRESSION(tau = 0.25),
  QUANTILE_REGRESSION(tau = 0.9),
  QUANTILE_REGRESSION(tau = 0.95)
)
