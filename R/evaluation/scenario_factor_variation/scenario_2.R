library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)
# source("R/simulations/default_models.R")
source("R/models_and_tests/model_computer.R")
source("R/helpers.R")


model_folder <- "results/shorter_gap_times_500"
store_name <- "plots/shorter_p_values_500.pdf"

models_to_exclude <- c("tweedie_var_power_1.5_link_power_0", "zero_inflate_wilcoxon", "quantile_regression_tau_0.5","zero_inflated_ttest","log_anova_c_10000")
model_files <- list.files(model_folder, full.names = TRUE)

#files without _qr
model_files <- model_files[!grepl("_qr", model_files)]

# model_file <- "results/shorter_gap_times/Scenario_2_k_1.5_s_1.RData"
# model_computer <- load_model_computer(model_file)
# add_model(model_computer, TWO_PART_WILCOXON_TEST(), recompute = TRUE)

df <- NULL
for (model_file in model_files) { # takes a while
  scenario_factor <- get_prefixed_number(model_file, "_s_")
  print(scenario_factor)

  model_computer <- load_model_computer(model_file)

  p_values <- get_value(model_computer, "p_value")
  sig_p_values <- colMeans(p_values < 0.05, na.rm = TRUE)
  print(sig_p_values)
  for (model in names(sig_p_values)) {
    if (model %in% models_to_exclude) {
      next
    }
    df <- rbind(df, c(scenario_factor = scenario_factor, model = model, value = unname(sig_p_values[model])))
  }

}


df <- data.frame(df)
df$value <- as.numeric(df$value)
df$scenario_factor <- as.numeric(df$scenario_factor)
df <- df[order(df$scenario_factor, decreasing = FALSE),]


source("R/models_and_tests/model_settings.R")

base_level <- 1.0

model_names_ <- order_models(unique(df$model))

colors_ <- setNames(unlist(lapply(model_names_, get_color)), model_names_)
line_types_ <- setNames(unlist(lapply(model_names_, get_line_style)), model_names_)
markers_ <- setNames(unlist(lapply(model_names_, get_marker)), model_names_)
labels_ <- lapply(model_names_, function(x) TeX(map_labels(x)))
named_labels <- setNames(labels_, model_names_)
brown_palette <- c("#8B4513", "#A0522D", "#D2691E", "#DEB887", "#CD853F", "#F4A460")

x_lab <- "Factor (Experimental/Control) for expected gap time between events"
y_lab <- "Proportion of Significant P-values"


transfromation <- scales::trans_new("log_reverse",
                                    transform = function(x) -log(x),
                                    inverse = function(x) exp(-x))

df <- df[df$scenario_factor >= 0.1,]
levels <- unique(df$scenario_factor)
levels.r <- round(levels, 1)
levels <- ifelse(levels == levels.r, levels.r, levels)

#remove some levels for the plot axis and markers
levels <- levels[!levels %in% c(0.4, 0.6, 0.7, 0.9)]

df$model_names <- sapply(df$model, function(x) TeX(map_labels(x)))

g <- ggplot(df, aes(x = scenario_factor, y = value, group = model)) +
  geom_line(aes(color = model, linetype = model), size = 1.1) +
  geom_point(data = subset(df, scenario_factor %in% levels), aes(color = model, shape = model), size = 3, stroke = 2) +
  scale_color_manual(values = colors_, labels = labels_, breaks = model_names_) +
  scale_linetype_manual(values = line_types_, labels = labels_, breaks = model_names_) +
  scale_shape_manual(values = markers_, labels = labels_, breaks = model_names_) +
  # scale_color_manual(values=brown_palette, labels = named_labels) +
  # scale_linetype_discrete(labels = named_labels) +
  # scale_shape_discrete(labels = labels_) +
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

