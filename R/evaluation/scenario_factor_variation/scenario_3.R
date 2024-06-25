library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)

source("R/models/model_results.R")
source("R/models/model_settings.R")
source("R/helpers.R")

model_folder <- "data/models/longer_event_durations"

model_files <- list.files(model_folder, full.names = TRUE)

# remove files with Tmp in it ...
model_files <- model_files[!grepl("Tmp", model_files)]

df <- NULL
for (model_file in model_files) { # takes a while
  scenario_factor <- get_prefixed_number(model_file, "_l_")
  print(scenario_factor)
  load(model_file , env = tmp <- new.env())
  model_results <- init_model_results(tmp$model_result_list)
  p_values <- model_results$get_values("p_value")
  sig_p_values <- colMeans(p_values < 0.05 , na.rm = TRUE )
  df <- rbind(df, c(scenario_factor = scenario_factor , sig_p_values))
}
df <- data.frame(df)
df <- df[order(df$scenario_factor,decreasing = TRUE),]

# Convert the data to a long format suitable for plotting with ggplot
results_long <- pivot_longer(df,cols=-scenario_factor , names_to = "model", values_to = "value")
levels <-  unique(results_long$scenario_factor)
# base_level = level_index where level= 1
base_level <- which(levels == 1)
results_long$scenario_factor <- factor(results_long$scenario_factor, levels = levels)
# Plot with line styles
# Significant P-values For different Factors for Shorter Event Gap Times
g <- ggplot(results_long, aes(x = scenario_factor, y = value, group=model)) +
  geom_line(aes(color = model, linetype = model),size=1.1) + # Map both color and linetype to model
  labs(x = "Factor (Experimental/Control) for expected gap time between events", y = "Proportion of Significant P-values", title = "", color = "Model", linetype = "Model") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 15) , legend.title = element_text(size = 0)) +
  theme(axis.text = element_text(size = 14 , face="bold"), axis.title  = element_text(size = 17, face="bold")) +
  theme(legend.key.width = unit(1, "cm")) +
  ylim(0, 1)
g
g  <- g +  scale_color_manual(values = unlist(sapply(sort(unique(results_long$model)),  function(x) get_color(x))),
                              labels = lapply(sort(unique(results_long$model)),  function(x) TeX(map_labels(x)))) +
  scale_linetype_manual(values = setNames(1:length(unique(results_long$model)), unique(results_long$model)),
                        labels = lapply(sort(unique(results_long$model)), function(x) TeX(map_labels(x))))+
  guides(fill=guide_legend(override.aes=list(size=15))) +
  geom_point(aes(color = model ), size=2.5 , show.legend = FALSE) +
  guides(color=guide_legend(nrow=2, byrow=TRUE)) +
  theme(legend.position='top') +
  geom_vline(xintercept = base_level, linetype="dotted",color = "black") +
  annotate("text", x = base_level+0.8, y = 0.92, label = "Equal expected \n even duration", angle = 0, color = "black" , size = 5)
g <-g+ annotate("segment", x =5.8, y = 0.05, xend = 1.7, yend = 0.05,
                arrow = arrow(type = "closed", length = unit(0.02, "npc")))+
  annotate("text", x = 3.7, y = 0.08, label = "Longer event durations in the experimental group", angle = 0, color = "black" , size = 4)
g


ggsave("longer_p_values.pdf", plot = g, width = 10, height = 5)

