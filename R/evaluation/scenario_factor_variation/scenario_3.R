library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)

source("R/evaluation/scenario_factor_variation/functions.R")
source("R/evaluation/config.R", local = (eval_config <- new.env()))

model_folder <- dirname(eval_config$DEFAULT_DURATION_VAR_FILE)
plot_name <- eval_config$DURATION_VARIATION_PLOT_PATH
model_settings <- get_plot_specs(DEFAULT_MODEL_PLOT_SETTINGS)


df <- get_p_value_df(model_folder, factor_prefix = "_l_")

df$scenario_factor <- as.numeric(df$scenario_factor)
df <- df[order(df$scenario_factor, decreasing = FALSE),]

base_level <- 1.0

x_lab <- "Factor (Experimental/Control) for longer expected episode durations"
y_lab <- "Proportion of Significant P-values"

colors_ <- model_settings$colors
line_types_ <- model_settings$line_styles
markers_ <- model_settings$markers
labels_ <- model_settings$TeX_labels
breaks <- names(colors_)

g <- ggplot(df, aes(x = scenario_factor, y = value, group = model)) +
  geom_line(aes(color = model, linetype = model), size = 1.1) +
  geom_point(aes(color = model,  shape = model), size = 3, stroke = 2) +
  scale_color_manual(values = colors_, labels = labels_, breaks = breaks) +
  scale_linetype_manual(values = line_types_, labels = labels_, breaks = breaks) +
  scale_shape_manual(values = markers_, labels = labels_, breaks = breaks) +
  labs(x = x_lab, y =y_lab, title = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 15), legend.title = element_text(size = 0)) +
  theme(axis.text = element_text(size = 14, face = "bold"), axis.title = element_text(size = 17, face = "bold")) +
  theme(legend.key.width = unit(2, "cm")) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
  guides(fill = guide_legend(override.aes = list(size = 15))) +
  theme(legend.position = 'top') +
  geom_vline(xintercept = base_level, linetype = "dotted", color = "black") +
  annotate("text", x = base_level+0.3, y = 0.92, label = "Equal expected \n episode durations", angle = 0, color = "black", size = 5)+
  annotate("segment", x = 1.6, y = 0.03, xend = 2 + 7.5, yend = 0.03, arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("text", x = 3.5, y = 0.05, label = "Longer episode durations in the experimental group", angle = 0, color = "black", size = 5)
  levels <- unique(df$scenario_factor)
  levels.r <- round(levels, 1)
  levels <- ifelse(levels == levels.r, levels.r,levels)

  values_to_remove <- c(0.5, 0.7, 0.9, 2/7)
  levels <- levels[!sapply(levels, function(x) any(abs(x - values_to_remove) < 0.0001))]

  transfromation <- scales::trans_new("log_reverse",
                                      transform = function(x) log(x),
                                      inverse = function(x) exp(x))

  g <- g + scale_x_continuous(trans=transfromation , breaks = levels , labels = levels )
  g
ggsave(plot_name, plot = g, width = 15, height = 10)
