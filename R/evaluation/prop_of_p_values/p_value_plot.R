library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)

source("R/models_and_tests/model_settings.R")

# Define the rescale_x function
rescale_x <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

p_value_plot_handler <- function(x_label = "P-Value", y_label = "CDF") {
  data <- data.frame()
  model_names <- c()
  colors <- c()
  model_rate_under_0.05 <- c()

  add <- function(model_p_values, model_name, color = NULL) {
    if (model_name %in% model_names) {
      print(paste0("Model ", model_name, " already added"))
      return()
    }

    model_p_values <- unlist(model_p_values)

    if (is.null(color)) {
      color <- get_color(model_name) # defaults to black
    }

    model_p_values <- sort(model_p_values)
    x <- rescale_x(1:length(model_p_values))
    stopifnot(x >= 0 & x <= 1)
    model_names <<- c(model_names, model_name)
    colors <<- c(colors, color)
    model_rate_under_0.05 <<- c(model_rate_under_0.05, sum(model_p_values < 0.05) / length(model_p_values))

    model_data <- data.frame(
      p_value = model_p_values,
      rescaled_id = x,
      model = model_name
    )
    data <<- rbind(data, model_data)
  }


  plot <- function(infer_colors = TRUE) {
    models_name_ <- model_names[order(model_rate_under_0.05, decreasing = TRUE)]
    if (infer_colors) {
      colors_ <- setNames(unlist(lapply(models_name_, get_color)), models_name_)
    }else {
      colors_ <- colors
    }
    line_types_ <- setNames(unlist(lapply(models_name_, get_line_style)), models_name_)
    labels_ <- lapply(models_name_, function(x) TeX(map_labels(x)))

    pos <- bottom_right <- c(0.8, 0.32)
    top_left <- c(0.16, 0.75)


    if (exists("scenario") && scenario == "equal") {
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
      scale_color_manual(values = colors_,
                         labels = labels_,
                         breaks = models_name_) +
      scale_linetype_manual(values = line_types_,
                            labels = labels_,
                            breaks = models_name_) +
      geom_vline(xintercept = 0.05, linetype = "dashed", color = "black") +
      annotate("text", x = 0.1, y = -0, label = "0.05", angle = 0, color = "black") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")
    return(g)
  }

  get_colors <- function() {
    return(colors)
  }

  save <- function(file_name = "p_values.pdf", file_path = "plots/p_values/", units = "cm",
                   width = 18, height = 15, infer_colors = TRUE, ...) {
    filename <- paste0(file_path, file_name)
    dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
    ggsave(filename, plot(infer_colors), units = units, width = width, height = height, ...)
  }

  return(list(plot = plot, add = add, get_colors = get_colors, save = save))
}

