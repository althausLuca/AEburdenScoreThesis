library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)

source("R/evaluation/config.R")


# Define the rescale_x function
rescale_x <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

p_value_plot_handler <- function(x_label = "P-Value", y_label = "CDF") {
  data <- data.frame()

  model_names <- c()
  colors <- c()
  line_styles <- c()
  model_rate_under_0.05 <- c()

  add <- function(model_p_values, model_name, color = "black" , linestyle = 1) {
    if (model_name %in% model_names) {
      print(paste0("Model ", model_name, " already added"))
      return()
    }

    model_p_values <- sort(model_p_values)
    x <- rescale_x(1:length(model_p_values))
    stopifnot(x >= 0 & x <= 1)

    model_names <<- c(model_names, model_name)
    colors <<- c(colors, color)
    line_styles <<- c(line_styles, linestyle)
    model_rate_under_0.05 <<- c(model_rate_under_0.05, sum(model_p_values < 0.05) / length(model_p_values))

    model_data <- data.frame(
      p_value = model_p_values,
      rescaled_id = x,
      model = model_name
    )

    data <<- rbind(data, model_data)
  }


  plot <- function(infer_colors = TRUE) {
    model_order <- order(model_rate_under_0.05, decreasing = TRUE)

    models_name_sorted <- model_names[model_order]
    line_styles_sorted <- line_styles[model_order]
    colors_sorted <- colors[model_order]
    models_names_tex <- lapply(models_name_sorted, function(x) TeX(x))

    pos <- bottom_right <- c(0.8, 0.32)
    top_left <- c(0.16, 0.75)

    if (exists("LEGEND_ON_TOP") && LEGEND_ON_TOP == "TRUE") {
      pos <- top_left
    }


    g <- ggplot(data, aes(x = p_value, y = rescaled_id, group = model)) +
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
    return(g)
  }

  get_colors <- function() {
    return(colors)
  }

  save <- function(file_name = "p_values.pdf", units = "cm",
                   width = 18, height = 15, infer_colors = TRUE, ...) {
    dir.create(dirname(file_name), recursive = TRUE, showWarnings = FALSE)
    ggsave(file_name, plot(infer_colors), units = units, width = width, height = height, ...)
  }

  return(list(plot = plot, add = add, get_colors = get_colors, save = save))
}

p_value_plot <- function(p_value_df, save = NULL , models_to_include = NULL) {
  p_values <- p_value_df
  model_reprs <- names(p_values)

  p_value_plot <- p_value_plot_handler()
  for (name in model_reprs) {
    if (!is.null(models_to_include) & !(name %in% models_to_include)) {
      next
    }
    p_vec <- unlist(p_values[[name]])
    color <- get_color(name)
    line_style <- get_line_style(name)
    label <- get_label(name)
    p_value_plot$add(p_vec, label , color , line_style)
  }

  if (!is.null(save)) {
    dir.create(dirname(save), recursive = TRUE, showWarnings = FALSE)
    p_value_plot$save(save)
  }

  plot <- p_value_plot$plot()
  return(plot)
}
