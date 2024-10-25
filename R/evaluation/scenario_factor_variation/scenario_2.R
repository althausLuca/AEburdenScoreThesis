library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)
# source("R/run_models/default_models.R")
source("R/models_and_tests/model_computer.R")
source("R/helpers.R")
source("R/evaluation/prop_of_p_values/functions.R")

model_folder <- "results_24_10/shorter_gap_times"
store_name <- "plots/shorter_p_values.pdf"

df <- get_p_value_df(model_folder, factor_prefix = "_s_")

df$scenario_factor <- as.numeric(df$scenario_factor)
df <- df[order(df$scenario_factor, decreasing = FALSE),]


base_level <- 1.0
x_lab <- "Factor (Experimental/Control) for expected gap time between events"
y_lab <- "Proportion of Significant P-values"

model_names_ <- order_models(unique(df$model))

colors_ <- setNames(unlist(lapply(model_names_, get_color)), model_names_)
line_types_ <- setNames(unlist(lapply(model_names_, get_line_style)), model_names_)
markers_ <- setNames(unlist(lapply(model_names_, get_marker)), model_names_)
labels_ <- lapply(model_names_, function(x) TeX(map_labels(x)))
named_labels <- setNames(labels_, model_names_)



transfromation <- scales::trans_new("log_reverse",
                                    transform = function(x) -log(x),
                                    inverse = function(x) exp(-x))

unique(df$scenario_factor)
levels <- unique(df$scenario_factor)

#remove some levels for the plot axis and markers
levels <- levels[!levels %in% c(0.4, 0.6, 0.7, 0.9)]
levels <- ifelse(levels == round(levels, 1), round(levels, 1) , levels)


breaks <- config$model_reprs
colors_ <- setNames(unlist(lapply(breaks, config$get_color)), breaks)
line_types_ <- setNames(unlist(lapply(breaks, config$get_line_style)), breaks)
markers_ <- setNames(unlist(lapply(breaks, config$get_marker)), breaks)
labels_ <- sapply(breaks, function(x) TeX(config$get_label(x)))

#, breaks = model_names_
g <- ggplot(df, aes(x = scenario_factor, y = value, group = model)) +
  geom_line(aes(color = model, linetype = model), size = 1.1) +
  geom_point(data = subset(df, scenario_factor %in% levels), aes(color = model, shape = model), size = 3, stroke = 2) +
  scale_color_manual(values = colors_, labels = labels_, breaks = breaks) +
  scale_linetype_manual(values = line_types_, labels = labels_, breaks = breaks) +
  scale_shape_manual(values = markers_, labels = labels_, breaks = breaks) +
  labs(x = x_lab, y = y_lab, title = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 15), legend.title = element_text(size = 0)) +
  theme(axis.text = element_text(size = 14, face = "bold"), axis.title = element_text(size = 17, face = "bold")) +
  theme(legend.key.width = unit(2, "cm")) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
  guides(fill = guide_legend(override.aes = list(size = 15))) +
  theme(legend.position = 'top') +
  geom_vline(xintercept = base_level, linetype = "dotted", color = "black") +
  annotate("text", x = base_level + 0.35, y = 0.92, label = "Equal expected \n gap time", angle = 0, color = "black", size = 5) +
  annotate("segment", x = 0.75, y = 0.05, xend = 0.12, yend = 0.05, arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("text", x = 0.3, y = 0.075, label = "More frequent events in the experimental group", angle = 0, color = "black", size = 5) +
  scale_x_continuous(trans = transfromation, breaks = levels, labels = levels)
g

ggsave(store_name, plot = g, width = 15, height = 10)

