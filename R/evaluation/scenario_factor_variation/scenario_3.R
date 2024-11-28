library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)

source("R/evaluation/scenario_factor_variation/functions.R")
source("R/evaluation/config.R", local = (eval_config <- new.env()))

# model_folder <- dirname(eval_config$DEFAULT_DURATION_VAR_FILE)
# plot_name <- eval_config$DURATION_VARIATION_PLOT_PATH
# plot_settings <- DEFAULT_MODEL_PLOT_SETTINGS

plot_duration_variation <- function(
  plot_settings = DEFAULT_MODEL_PLOT_SETTINGS,
  model_folder = dirname(eval_config$DEFAULT_DURATION_VAR_FILE),
  plot_name = eval_config$DURATION_VARIATION_PLOT_PATH,
  omit_arrow = FALSE){

  model_settings <- get_plot_specs(plot_settings)

  df <- get_p_value_df(model_folder, factor_prefix = "_l_")

  df$scenario_factor <- as.numeric(df$scenario_factor)
  df <- df[order(df$scenario_factor, decreasing = FALSE),]

  base_level <- 1.0

  x_lab <- "Factor (Experimental/Control) for longer expected episode durations"
  y_lab <- "Proportion of Significant P-values"

  levels <- unique(df$scenario_factor)
  # round the digits

  values_to_remove <- c(0.5, 0.7, 0.9, 2 / 7, 1.25, 1 / 1.25)
  levels <- levels[!levels %in% values_to_remove]
  # Create formatted labels based on condition
  level_labels <- sapply(levels, function(x) {
    if (x >= 1) {
      TeX(paste0("${\\phantom{^{-1}}}", x, "\\phantom{^{-1}}$"))
    } else {
      TeX(paste0("$^{\\phantom{^{-1}}}", 1 / x, "^{-1}$"))
    }
  })


  transfromation <- scales::trans_new("log_reverse",
                                      transform = function(x) log(x),
                                      inverse = function(x) exp(x))


  colors_ <- model_settings$colors
  line_types_ <- model_settings$line_styles
  markers_ <- model_settings$markers
  labels_ <- model_settings$TeX_labels
  breaks <- names(colors_)

  g <- ggplot(df, aes(x = scenario_factor, y = value, group = model)) +
    geom_line(aes(color = model, linetype = model), size = 1.1) +
    geom_point(aes(color = model, shape = model), size = 3, stroke = 2) +
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
    annotate("text", x = base_level + 0.3, y = 0.92, label = "Equal expected \n  episode durations", angle = 0, color = "black", size = 5)

  if (!omit_arrow){
    g <- g + geom_segment(aes(x = 1.6, y = 0.02, xend = 2 + 7.5, yend = 0.02), arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
      annotate("text", x = 3.6, y = 0.04, label = "Longer episode durations in the experimental group", angle = 0, color = "black", size = 5)
  }

  g <- g + scale_x_continuous(trans = transfromation, breaks = levels, labels = level_labels)
  g
  ggsave(plot_name, plot = g, width = 15, height = 10)
}