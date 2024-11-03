source("R/models_and_tests/models_and_tests.R")
source("R/run_models/config.R", local = (run_config <- new.env()))
source("R/evaluation/model_settings.R")

# default should be the same as in run_models/config.R  (run_config$MODEL_RESULT_PATH)
MODEL_DATA_PATH <- run_config$MODEL_RESULT_PATH

PLOT_PATH <- "plots/method_evaluation/"

#scenario_factor_variation default files
DEFAULT_DURATION_VAR_FILE <- paste0(MODEL_DATA_PATH, "longer_event_durations/s3_k_1.5_l_3.5.RData")
DEFAULT_GAP_TIME_VAR_FILE <- paste0(MODEL_DATA_PATH, "shorter_gap_times/s2_k_1.5_s_0.5.RData")
EQUAL_SETTIGS_FILE <- paste0(MODEL_DATA_PATH, "s1_k_1.5.RData")
TRIAL_SIZE_VARIATION_PATH <- paste0(MODEL_DATA_PATH,"sample_size_variation/")


DURATION_VARIATION_PLOT_PATH <- paste0(PLOT_PATH, "duration_variation.pdf")
GAP_TIME_VARIATION_PLOT_PATH <- paste0(PLOT_PATH, "gap_time_variation.pdf")
SEVERITY_INCREASE_PLOT_PATH <- paste0(PLOT_PATH, "severity_increase.pdf")

# models to plot for p_value plots, the order matters
#model, latex compatible label, color, line_style, marker
# default_models_to_plot <- list(
#   list(WILCOXON_TEST(), "Wilcoxon Test", "darkgreen", "dashed", 6),
#   list(PERMUTATION_TEST(), "Permutation Test", "yellow3", "dotted", 5),
#   list(ANOVA(), "ANOVA", "purple", "solid", 5),
#   list(LOG_ANOVA(c = 0.001), "Log-ANOVA $$$_{c=0.001}$", "royalblue4", "dotdash", 1),
#   list(LOG_ANOVA(c = 1), "Log-ANOVA $$$_{c=1}$", "navy", "dotted", 2),
#   list(TWEEDIE_REGRESSION(xi = "infer"), "Tweedie Regression", "tomato4", "dotted", 4),
#   list(ZERO_INFLATED_GAMMA(), "Zero-Inflated Gamma", "red4", "dotdash", 5),
#   list(ZERO_INFLATED_LOGNORMAL(), "Zero-Inflated Lognormal", "maroon1", "dashed", 6),
#   list(TWO_PART_WILCOXON_TEST(), "Two-Part Wilcoxon Test", "orangered1", "1F", 1),
#   #list(TWO_PART_T_TEST(), "Two-Part T-Test", "red", "dotted", 4),
#   list(TWO_PART_T_TEST(use_welch=T), "Two-Part Welch-Test", "red", "dotted", 4),
#   list(QUANTILE_REGRESSION(tau = 0.5), "Median Regression", "lightpink4", "solid", 3),
#   list(QUANTILE_REGRESSION(tau = 0.75), "Quantile Regression $$$_{\\tau=0.75}$", "lightpink1", "dotted", 1)
# )
#
#
# model_reprs <- sapply(default_models_to_plot, function(x) x[[1]]$repr)
# model_labels <- sapply(default_models_to_plot, function(x) x[[2]])
# model_colors <- sapply(default_models_to_plot, function(x) x[[3]])
# model_line_styles <- sapply(default_models_to_plot, function(x) x[[4]])
# model_markers <- sapply(default_models_to_plot, function(x) x[[5]])
#

#check if default files exit
if (!file.exists(DEFAULT_GAP_TIME_VAR_FILE)) {
  print(paste0("File ", DEFAULT_GAP_TIME_VAR_FILE, " does not exist"))
}
if (!file.exists(DEFAULT_DURATION_VAR_FILE)) {
  print(paste0("File ", DEFAULT_DURATION_VAR_FILE, " does not exist"))
}
if (!file.exists(EQUAL_SETTIGS_FILE)) {
  print(paste0("File ", EQUAL_SETTIGS_FILE, " does not exist"))
}

dir.create(PLOT_PATH, recursive = TRUE , showWarnings = FALSE)
