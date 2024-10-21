library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)

source("R/helpers.R")
source("R/models_and_tests/model_computer.R")
source("R/trials/trial_data.R")

result_folder <- "results/more_severe_events/"
models_to_exclude <- c("tweedie_var_power_1.5_link_power_0", "zero_inflate_wilcoxon", "quantile_regression_tau_0.5","zero_inflated_ttest","log_anova_c_10000")
# list folder
model_files <- list.files(result_folder, full.names = TRUE)
model_files <- model_files[!grepl("_qr", model_files)]

model_file.1 <- "results/more_severe_events/all_mostly_severe_experimental.RData"
model_computer.1 <- load_model_computer(model_file.1)
summary.trial_data(model_computer.1$trial_data)

model_file.2 <-"results/more_severe_events/next_level_experimental.RData"
model_computer.2 <- load_model_computer(model_file.2)
summary.trial_data(model_computer.2$trial_data)

mean(get_value(model_computer.1, "p_value")[,3] < 0.05)
mean(get_value(model_computer.2, "p_value")[,3] < 0.05)

df <- NULL
for (model_file in model_files) { # takes a while
  scenario_factor <- basename(model_file)
  scenario_factor <- gsub(".RData", "", scenario_factor)
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
df_ <- df

df <- df_

df <- data.frame(df)
unique(df$scenario_factor)
factor_names <- c("default" = "Equal settings",
                  "next_level_experimental" = "Experimental next severity level",
                  "all_mostly_severe_experimental" = "Experimental all mostly severe",
                  "all_severe_experimental" = "Experimental all severe",
                  "all_severe_experimental_all_mild_control" = "Experimental all sever \n Control all mild"
)


df$value <- as.numeric(df$value)

converted_factors <- unname(factor_names[df$scenario_factor])

df$scenario_factor <- factor(converted_factors, levels = unname(factor_names))

df <- df[is.na(df$scenario_factor) == FALSE,]

# df <- df[order(df$scenario_factor, decreasing = FALSE),]


source("R/models_and_tests/model_settings.R")


model_names_ <- order_models(unique(df$model))

colors_ <- setNames(unlist(lapply(model_names_, get_color)), model_names_)
line_types_ <- setNames(unlist(lapply(model_names_, get_line_style)), model_names_)
markers_ <- setNames(unlist(lapply(model_names_, get_marker)), model_names_)
labels_ <- lapply(model_names_, function(x) TeX(map_labels(x)))


x_lab <- "Severity Difference"
y_lab <- "Proportion of Significant P-values"


# df <- df[df$scenario_factor > 0.1,]
levels <- unique(df$scenario_factor)
# levels.r <- round(levels, 1)
# levels <- ifelse(levels == levels.r, levels.r, levels)

#remove some levels for the plot axis and markers
levels <- levels[!levels %in% c(0.4,0.6, 0.7, 0.9)]

base_level <- "severity.RData"


g <- ggplot(df, aes(x = scenario_factor, y = value, group = model)) +
  geom_line(aes(color = model, linetype = model), size = 1.1) +
  geom_point(data= subset(df, scenario_factor %in% levels), aes(color = model, shape = model), size = 3, stroke = 2) +
  scale_color_manual(values = colors_, labels = labels_, breaks = model_names_) +
  scale_linetype_manual(values = line_types_, labels = labels_, breaks = model_names_) +
  scale_shape_manual(values = markers_, labels = labels_, breaks = model_names_) +
  labs(x = x_lab, y = y_lab, title = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 15), legend.title = element_text(size = 0)) +
  theme(axis.text = element_text(size = 14, face = "bold"), axis.title = element_text(size = 17, face = "bold")) +
  theme(legend.key.width = unit(2, "cm")) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
  guides(fill = guide_legend(override.aes = list(size = 15))) +
  theme(legend.position = 'top') #+
  # theme(axis.text.x = element_text(angle = 45, hjust = 1))
g
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
 geom_vline(xintercept = 1, linetype = "dotted", color = "black") +
  annotate("text", x = 1 + 0.35, y = 0.92, label = "Equal expected \n severity", angle = 0, color = "black", size = 5) +
  annotate("segment", x = 1.1, y = 0.5, xend = 3, yend = 0.5, arrow = arrow(type = "closed", length = unit(0.02, "npc")))+
  annotate("text", x = 2, y = 0.53, label = "Higher severity in experimental group", angle = 0, color = "black", size = 5)
  # scale_x_continuous(trans = transfromation, breaks = levels, labels = levels)

store_name <- "more_severe_events.pdf"
ggsave(store_name, plot = g, width = 15, height = 10)

source("R/evaluation/long_df_to_table.R")
print(long_df_to_table(df))
