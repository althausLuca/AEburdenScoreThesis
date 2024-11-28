library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)

source("R/evaluation/scenario_factor_variation/functions.R")
source("R/evaluation/config.R", local = (eval_config <- new.env()))

# model_folder <- dirname(eval_config$DEFAULT_GAP_TIME_VAR_FILE)
# plot_name <- eval_config$GAP_TIME_VARIATION_PLOT_PATH
# plot_settings = DEFAULT_MODEL_PLOT_SETTINGS

plot_gap_time_variation <- function(
  plot_settings = DEFAULT_MODEL_PLOT_SETTINGS,
  model_folder = dirname(eval_config$DEFAULT_GAP_TIME_VAR_FILE),
  plot_name = eval_config$GAP_TIME_VARIATION_PLOT_PATH,
  omit_arrow = FALSE
) {
  model_settings <- get_plot_specs(plot_settings)

  df <- get_p_value_df(model_folder, factor_prefix = "_s_")

  df[df$model == "quantile_regression_tau_0.5_xy",]

  df$scenario_factor <- as.numeric(df$scenario_factor)
  df <- df[order(df$scenario_factor, decreasing = FALSE),]

  base_level <- 1.0
  x_lab <- "Factor (Experimental/Control) for expected gap time between event episodes"
  y_lab <- "Proportion of Significant P-values"
  levels_to_exclude <- c(0.6, 0.7, 0.9, 1.25, 1 / 1.25)


  transfromation <- scales::trans_new("log_reverse",
                                      transform = function(x) -log(x),
                                      inverse = function(x) exp(-x))

  levels <- unique(df$scenario_factor)

  #remove some levels for the plot axis and markers
  levels <- levels[!levels %in% levels_to_exclude]

  # Create formatted labels based on condition
  level_labels <- sapply(levels, function(x) {
    if (x >= 1) {
      TeX(paste0("${\\phantom{^{-1}}}", x, "\\phantom{^{-1}}$"))
    } else {
      TeX(paste0("$^{\\phantom{^{-1}}}", 1 / x, "^{-1}$"))
    }
  })

  # # round the digits
  # levels <- ifelse(levels == round(levels, 3),levels, round(levels, 3))

  colors_ <- model_settings$colors
  line_types_ <- model_settings$line_styles
  markers_ <- model_settings$markers
  labels_ <- model_settings$TeX_labels
  breaks <- names(colors_)

  g <- ggplot(df, aes(x = scenario_factor, y = value, group = model)) +
    geom_line(aes(color = model, linetype = model), size = 1.1) +
    geom_point(data = df, aes(color = model, shape = model), size = 3, stroke = 2) +
    scale_color_manual(values = colors_, labels = labels_, breaks = breaks) +
    scale_linetype_manual(values = line_types_, labels = labels_, breaks = breaks) +
    scale_shape_manual(values = markers_, labels = labels_, breaks = breaks) +
    labs(x = x_lab, y = y_lab, title = "") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 15), legend.title = element_text(size = 0)) +
    theme(axis.text = element_text(size = 15), axis.title = element_text(size = 17, face = "bold")) +
    theme(legend.key.width = unit(2, "cm")) +
    guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
    guides(fill = guide_legend(override.aes = list(size = 15))) +
    theme(legend.position = 'top') +
    geom_vline(xintercept = base_level, linetype = "dotted", color = "black") +
    annotate("text", x = base_level + 0.35, y = 0.92, label = "Equal expected \n gap time", angle = 0, color = "black", size = 5)

  if (!omit_arrow) {
    g <- g +
      geom_segment(aes(x = 1.6, y = 0.02, xend = 2 + 7.5, yend = 0.02), arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
      annotate("text", x = 3.6, y = 0.04, label = "Shorter gap times in the experimental group", angle = 0, color = "black", size = 5)
  }
  g <- g + scale_x_continuous(trans = transfromation, breaks = levels, labels = level_labels)

  ggsave(plot_name, plot = g, width = 15, height = 10)
}