library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)

source("R/models_and_tests/model_computer.R")

model_folder <- "results/longer_event_durations"
model_files <- list.files(model_folder, full.names = TRUE)

source("R/helpers.R")


df <- NULL
for (model_file in model_files) { # takes a while
  scenario_factor <- get_prefixed_number(model_file, "_l_")
  print(scenario_factor)
  model_computer <- load_model_computer(model_file)

  p_values <- get_value(model_computer, "p_value")
  sig_p_values <- colMeans(p_values < 0.05, na.rm = TRUE)
  print(sig_p_values)
  for (model in names(sig_p_values)) {
    df <- rbind(df, c(scenario_factor = scenario_factor, model = model, value = unname(sig_p_values[model])))
  }

}


df <- data.frame(df)
df$value <- as.numeric(df$value)
df$scenario_factor <- as.numeric(df$scenario_factor)
df <- df[order(df$scenario_factor, decreasing = FALSE),]

models_to_exclude <- c("tweedie_var_power_1.5_link_power_0")
df <- df[!(df$model %in% models_to_exclude),]


source("R/models_and_tests/model_settings.R")

base_level <- 1.0

model_names_ <- sort(unique(df$model))
colors_ <- setNames(unlist(lapply(model_names_, get_color)), model_names_)
line_types_ <- setNames(unlist(lapply(model_names_, get_line_style)), model_names_)
markers_ <- setNames(unlist(lapply(model_names_, get_marker)), model_names_)
labels_ <- lapply(model_names_, function(x) TeX(map_labels(x)))

x_lab <- "Factor (Experimental/Control) for expected gap time between events"
y_lab <- "Proportion of Significant P-values"
g <- ggplot(df, aes(x = scenario_factor, y = value, group = model)) +
  geom_line(aes(color = model, linetype = model), size = 1.1) +
  geom_point(aes(color = model,  shape = model), size = 3, stroke = 2) +
  scale_color_manual(values = colors_, labels = labels_, breaks = model_names_) +
  scale_linetype_manual(values = line_types_, labels = labels_, breaks = model_names_) +
  scale_shape_manual(values = markers_, labels = labels_, breaks = model_names_) +
  labs(x = x_lab, y =y_lab, title = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 15), legend.title = element_text(size = 0)) +
  theme(axis.text = element_text(size = 14, face = "bold"), axis.title = element_text(size = 17, face = "bold")) +
  theme(legend.key.width = unit(2, "cm")) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
  guides(fill = guide_legend(override.aes = list(size = 15))) +
  theme(legend.position = 'top') +
  geom_vline(xintercept = base_level, linetype = "dotted", color = "black") +
  annotate("text", x = base_level+0.3, y = 0.92, label = "Equal expected \n even duration", angle = 0, color = "black", size = 5)+
  annotate("segment", x = 1.6, y = 0.03, xend = 2 + 5, yend = 0.03, arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("text", x = 3.5, y = 0.06, label = "Longer event durations in the experimental group", angle = 0, color = "black", size = 4)
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
ggsave("longer_p_values.pdf", plot = g, width = 15, height = 10)






model_computer