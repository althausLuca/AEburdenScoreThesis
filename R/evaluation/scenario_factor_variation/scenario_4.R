library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)

source("R/evaluation/scenario_factor_variation/functions.R")
source("R/evaluation/config.R", local = (eval_config <- new.env()))

severity_factor_names <- c("default" = "Equal settings",
                           "next_level_experimental" = "Experimental next severity level",
                           "all_mostly_severe_experimental" = "Experimental all mostly severe",
                           "all_severe_experimental" = "Experimental all severe",
                           "all_severe_experimental_all_mild_control" = "Experimental all severe \n Control all mild"
)

# plot_severity_variation()
plot_severity_variation <- function(
  plot_settings = DEFAULT_MODEL_PLOT_SETTINGS,
  model_folder = paste0(eval_config$MODEL_DATA_PATH, "more_severe_events"),
  plot_name = eval_config$SEVERITY_INCREASE_PLOT_PATH,
  x_lab =  "Severity Pattern",
  y_lab = "Proportion of Significant P-values"
) {

  model_settings <- get_plot_specs(plot_settings)
  model_reps <- model_settings$repr

  # list folder
  model_files <- list.files(model_folder, full.names = TRUE)

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
      if (model %in% model_reps) {
        df <- rbind(df, c(scenario_factor = scenario_factor, model = model, value = unname(sig_p_values[model])))
      }
    }

  }
  df <- data.frame(df)

  df$value <- as.numeric(df$value)

  levels <- unname(severity_factor_names)
  converted_factors <- unname(severity_factor_names[df$scenario_factor])

  df$scenario_factor <- factor(converted_factors, levels = levels)

  df <- df[is.na(df$scenario_factor) == FALSE,]


  colors_ <- model_settings$colors
  line_types_ <- model_settings$line_styles
  markers_ <- model_settings$markers
  labels_ <- model_settings$TeX_labels
  breaks <- names(colors_)

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
    theme(legend.position = 'top') #+
  # theme(axis.text.x = element_text(angle = 45, hjust = 1))
  g <- g +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_vline(xintercept = 1, linetype = "dotted", color = "black") +
    annotate("text", x = 1 + 0.35, y = 0.92, label = "Equal expected \n severity", angle = 0, color = "black", size = 5) +
    annotate("segment", x = 1.1, y = 0.5, xend = 3, yend = 0.5, arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
    annotate("text", x = 2, y = 0.53, label = "Higher severity in experimental group", angle = 0, color = "black", size = 5)
  # scale_x_continuous(trans = transfromation, breaks = levels, labels = levels)

  g#
  ggsave(plot_name, plot = g, width = 15, height = 10)

  return(list(plot = g, df = df))
}
