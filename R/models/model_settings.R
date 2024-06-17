# Define constants for the different models used in the geheration and analysis
LOG_ANOVA <- "anova"
LM <- "lm"
TWEEDIE <- "tweedie"
QUANTILE_REGRESSION <- "quantile_regression"

MODEL_COLORS <- setNames(
  c("blue", "green", "black", "orange"),
  c(LOG_ANOVA, LM, TWEEDIE,  QUANTILE_REGRESSION)
)

MODEL_LINETYPES <- setNames(
  c("solid", "twodash", "dotted", "dotdash"),
  c(LOG_ANOVA, LM, TWEEDIE,  QUANTILE_REGRESSION)
)
