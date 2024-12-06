# models to plot for p_value plots, the order matters
library(latex2exp)

#model, latex compatible label, color, line_style, marker
DEFAULT_MODEL_PLOT_SETTINGS <- list(
  list(WILCOXON_TEST(), "Wilcoxon Test", "darkgreen", "dashed", 6),
  list(PERMUTATION_TEST(), "Permutation Test", "yellow3", "dotted", 5),
  list(ANOVA(), "ANOVA", "purple", "solid", 5),
  list(LOG_ANOVA(c = 0.001), "Log-ANOVA $$$_{c=0.001}$", "royalblue4", "dotdash", 1),
  list(LOG_ANOVA(c = 1), "Log-ANOVA $$$_{c=1}$", "navy", "dotted", 2),
  list(TWEEDIE_REGRESSION(xi = "infer"), "Tweedie Regression", "tomato4", "dotted", 4),
  list(ZERO_INFLATED_GAMMA(), "Zero-Inflated Gamma", "lightpink1", "dotdash", 5),
  list(ZERO_INFLATED_LOGNORMAL(), "Zero-Inflated Lognormal", "maroon1", "dotted", 6),
  list(TWO_PART_WILCOXON_TEST(), "Two-Part Wilcoxon Test", "orangered1", "12", 1),
  #list(TWO_PART_T_TEST(), "Two-Part T-Test", "red", "dotted", 4),
  list(TWO_PART_T_TEST(use_welch = T), "Two-Part Welch-Test", "red", "dotted", 4),
  list(QUANTILE_REGRESSION(tau = 0.5), "Median Regression", "lightpink4", "solid", 3),
  list(QUANTILE_REGRESSION(tau = 0.75), "Quantile Regression $$$_{\\tau=0.75}$", "red4", "dotted", 1)
)


brown_palette <- c("#8B4513", "#A0522D", "#D2691E", "#DEB887", "#CD853F", "#F4A460")
# define QR modle faur tau in 0.05, 0.1,  0.25, 0.5, 0.75, 0.9 0.95
QR_MODEL_PLOT_SETTINGS <- list(
  list(QUANTILE_REGRESSION(tau = 0.05), "Quantile Regression $$$_{\\tau=0.05}$", brown_palette[1], "dashed", 2),
  list(QUANTILE_REGRESSION(tau = 0.1), "Quantile Regression $$$_{\\tau=0.1}$", brown_palette[2], "dotdash", 4),
  list(QUANTILE_REGRESSION(tau = 0.25), "Quantile Regression $$$_{\\tau=0.25}$", brown_palette[3], "1F", 5),
  list(QUANTILE_REGRESSION(tau = 0.5), "Median Regression", "lightpink4", "solid", 3),
  list(QUANTILE_REGRESSION(tau = 0.75), "Quantile Regression $$$_{\\tau=0.75}$", "red4", "dotted", 1),
  list(QUANTILE_REGRESSION(tau = 0.9), "Quantile Regression $$$_{\\tau=0.9}$", brown_palette[4], "dashed", 6),
  list(QUANTILE_REGRESSION(tau = 0.95), "Quantile Regression $$$_{\\tau=0.95}$", brown_palette[5], "dotdash", 2)
)

#' Get the plot specs for the models
#'
#' @param model_reprs The model representations
#' @param settings The settings for the models
#' @return A list with the latex labels, colors, line styles and markers
get_plot_specs <- function(settings = DEFAULT_MODEL_PLOT_SETTINGS) {
  result <- list()

  repr <- sapply(settings, function(x) x[[1]]$repr)

  result$repr <- repr
  result$latex_labels <- setNames(sapply(settings, function(x) x[[2]]), repr)
  result$colors <-  setNames(sapply(settings, function(x) x[[3]]), repr)
  result$line_styles <- setNames(sapply(settings, function(x) x[[4]]), repr)
  result$markers <- setNames(sapply(settings, function(x) x[[5]]),repr)
  result$TeX_labels <- setNames(sapply(result$latex_labels, function(x) TeX(x)), repr)

  return(result)
}
