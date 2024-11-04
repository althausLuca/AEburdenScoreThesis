library(ggplot2)

p_value_plot <- function(model_computer,
                         model_plot_settings = DEFAULT_MODEL_PLOT_SETTINGS,
                         save = NULL,
                         legend_top_left = F,
                         na.rm = F,
                         x_label = "P-value",
                         y_label = "CDF")
{
  plot_specs <- get_plot_specs(model_plot_settings)
  p_values <- get_value(model_computer, "p_value")

  p_values <- subset(p_values, select = plot_specs$repr)
  x <- 1:(nrow(p_values)+1)/nrow(p_values)


  #legend position
  pos <- c(0.8, 0.32) # bottom right
  if (legend_top_left) {
    pos <- c(0.16, 0.75) # top left
  }



  data <- lapply(x , function(i) { colMeans(p_values < i, na.rm = na.rm) })
  #to long format
  data <- do.call(rbind, data)
  long_df <- data.frame(
    x = rep(x, each = ncol(p_values)),
    model = rep(names(p_values), length(x)),
    value = as.vector(t(data))
  )
  head(long_df)

  col_means_alpha_0.05 <- colMeans(p_values < 0.05, na.rm = na.rm)
  model_names <- names(col_means_alpha_0.05)

  model_order <- order(col_means_alpha_0.05, decreasing = TRUE)
  models_name_sorted <- model_names[model_order]

  models_name_sorted <- models_name_sorted[models_name_sorted %in% plot_specs$repr]

  colors_sorted <- plot_specs$colors[models_name_sorted]
  line_styles_sorted <- plot_specs$line_styles[models_name_sorted]
  models_names_tex <- plot_specs$TeX_labels[models_name_sorted]


  g <- ggplot(long_df, aes(x = x, y = value, group = model)) +
    geom_line(aes(color = model, linetype = model), size = 1.1) +
    labs(x = x_label, y = y_label, title = "") +
    theme_minimal() +
    theme(
      legend.position = pos,
      plot.margin = margin(0, 70, 0, 0),
      legend.margin = margin(0, -100, 0, 0),
      legend.title = element_blank(),
      legend.text = element_text(size = 11),
      # legend.box.just = "left",
      axis.text = element_text(size = 13, face = "bold"),
      axis.title = element_text(size = 16, face = "bold")
    ) +
    scale_color_manual(values = colors_sorted,
                       labels = models_names_tex,
                       breaks = models_name_sorted) +
    scale_linetype_manual(values = line_styles_sorted,
                          labels = models_names_tex,
                          breaks = models_name_sorted) +
    geom_vline(xintercept = 0.05, linetype = "dashed", color = "black") +
    annotate("text", x = 0.1, y = -0, label = "0.05", angle = 0, color = "black") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")
  g

  if (!is.null(save)) {
    ggsave(save, plot = g, units = "cm", width = 18, height = 15)
  }
  return(g)
}
