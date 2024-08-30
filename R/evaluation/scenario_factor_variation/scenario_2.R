library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)

source("R/models_and_tests/model_computer.R")
source("R/helpers.R")


model_folder <- "results/shorter_gap_times"
model_files <- list.files(model_folder, full.names = TRUE)



# remove files with Tmp in it ...

df <- NULL
for (model_file in model_files) { # takes a while
  scenario_factor <- get_prefixed_number(model_file, "_s_")
  print(scenario_factor)
  model_computer <- load_model_computer(model_file )

  p_values <- get_value(model_computer,"p_value")
  sig_p_values <- colMeans(p_values < 0.05, na.rm = TRUE)
  print(sig_p_values)
  for( model in names(sig_p_values) ){
    df <- rbind(df, c(scenario_factor = scenario_factor, model = model , value = unname(sig_p_values[model])))
  }

}


df <- data.frame(df)
df$value <- as.numeric(df$value)
df$scenario_factor <- as.numeric(df$scenario_factor)
df <- df[order(df$scenario_factor, decreasing = FALSE),]


# Convert the data to a long format suitable for plotting with ggplot

# levels <- unique(df$scenario_factor)
# base_level = level_index where level= 1
# base_level <- which(levels == 1)
# df$scenario_factor <- factor(df$scenario_factor, levels = levels)
# df$scenario_factor <- as.numeric(df$scenario_factor)


source("R/models_and_tests/model_settings.R")

base_level <- 1.0
models <- sort(unique(df$model))
color_values <- unname(sapply(models , get_color))
labels <- unlist(lapply(models, function(x) TeX(map_labels(x))))
names(labels) <- models

lines_types <- setNames(1:length(models), models)

g <- ggplot(df, aes(x = scenario_factor, y = value, group = model)) +
  geom_line(aes(color = model, linetype = model), size = 1.1) + # Map both color and linetype to model
  labs(x = "Factor (Experimental/Control) for expected gap time between events", y = "Proportion of Significant P-values", title = "", color = "Model", linetype = "Model") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 15), legend.title = element_text(size = 0)) +
  theme(axis.text = element_text(size = 14, face = "bold"), axis.title = element_text(size = 17, face = "bold")) +
  theme(legend.key.width = unit(1, "cm")) +
  ylim(0, 1) + scale_color_manual(values = color_values, labels =labels) + scale_linetype_manual(values = lines_types, labels = labels)
g
g <- g +


  guides(fill = guide_legend(override.aes = list(size = 15))) +
  geom_point(aes(color = model), size = 2.5, show.legend = FALSE) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.position = 'top') +
  geom_vline(xintercept = base_level, linetype = "dotted", color = "black") +
  annotate("text", x = base_level + 0.35, y = 0.92, label = "Equal expected \n gap time", angle = 0, color = "black", size = 5)
g
g <- g +
  annotate("segment", x = 0.75, y = 0.05, xend = 0.12, yend = 0.05,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("text", x = 0.3, y = 0.075, label = "More frequent events in the experimental group", angle = 0, color = "black", size = 5)
g


levels <- unique(df$scenario_factor)
levels.r <- round(levels, 1)
levels <- ifelse(levels == levels.r, levels.r,levels)
#remove 0.8
levels <- levels[!levels %in% c(0.5,0.7,0.9)]

transfromation <- scales::trans_new("log_reverse",
                                    transform = function(x) -log(x),
                                    inverse = function(x) exp(-x))

g <- g + scale_x_continuous(trans=transfromation , breaks = levels , labels = levels )
g
# ggsave("shorter_p_values.pdf", plot = g, width = 14, height = 7)

