library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)

source("R/models/model_settings.R")

# Define the rescale_x function
rescale_x <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


#' P-Value Plot Handler
#'
#' Creates a handler for managing and plotting p-value CDF plots for different models.
#'
#' @return A list of functions for adding data, plotting, retrieving colors, and saving the plot.
#'
#' @section Functions:
#' \describe{
#'   \item{\code{add(model_p_values, model_name, color = NULL)}}{
#'     Adds a model's p-values to the plot data.
#'     \itemize{
#'       \item \code{model_p_values}: A numeric vector of p-values for the model.
#'       \item \code{model_name}: A character string representing the name of the model.
#'       \item \code{color}: (Optional) A character string representing the color for the model in the plot. Defaults to black.
#'     }
#'   }
#'   \item{\code{plot()}}{
#'     Generates and returns the ggplot object for the p-value CDF plot.
#'   }
#'   \item{\code{get_colors()}}{
#'     Returns a vector of colors used for the models.
#'   }
#'   \item{\code{save(file_name = "p_values.pdf", file_path = "plots/p_values/", units = "cm", width = 15, height = 15, ...)}}{
#'     Saves the plot to a file.
#'     \itemize{
#'       \item \code{file_name}: The name of the file to save the plot as. Default is "p_values.pdf".
#'       \item \code{file_path}: The directory path to save the file in. Default is "plots/p_values/".
#'       \item \code{units}: The units for the plot dimensions. Default is "cm".
#'       \item \code{width}: The width of the plot. Default is 15.
#'       \item \code{height}: The height of the plot. Default is 15.
#'       \item \code{...}: Additional arguments passed to \code{ggsave}.
#'     }
#'   }
#' }
p_value_plot_handler <- function() {
  data <- data.frame()
  model_names <- c()
  colors <- c()

  add <- function(model_p_values, model_name, color = NULL) {

    if (is.null(color)) {
      color <- get_color(model_name)# defaults to black
    }

    model_p_values <- sort(model_p_values)
    x <- rescale_x(1:length(model_p_values))
    stopifnot(x>=0 & x<=1)
    model_names <<- c(model_names, model_name)
    colors <<- c(colors, color)

    model_data <- data.frame(
      p_value = model_p_values,
      rescaled_id = x,
      model = model_name
    )
    data <<- rbind(data, model_data)
  }

  plot <- function() {
    g <- ggplot(data, aes(x = p_value, y = rescaled_id, group = model)) +
      geom_line(aes(color = model, linetype = model), size = 1.1) +
      labs(x = "P-Value", y = "CDF", title = "") +
      theme_minimal() +
      theme(
        legend.position = c(0.99, 0.01),
        legend.justification = c(1, -0.1),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.box.just = "left",
        axis.text = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 16, face = "bold")
      ) +
      scale_color_manual(values = colors[order(model_names)],
                         labels = lapply(sort(model_names), function(x) TeX(map_labels(x)))) +
      scale_linetype_manual(values = setNames(1:length(model_names), model_names),
                            labels = lapply(sort(model_names), function(x) TeX(map_labels(x)))) +
      geom_vline(xintercept = 0.05, linetype = "dashed", color = "red") +
      annotate("text", x = 0.1, y = -0, label = "0.05", angle = 0, color = "red")+
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")
    return(g)
  }

  get_colors <- function(){
    return(colors)
  }

  save <- function(file_name = "p_values.pdf", file_path = "plots/p_values/" , units="cm", width=15, height=15, ...){
    dir.create(file_path , recursive = TRUE, showWarnings = FALSE)
    filename <- paste0(file_path, file_name)
    ggsave(filename, plot(), units = units, width = width, height = height, ...)
  }

  return(list(plot = plot, add = add , get_colors = get_colors , save =  save ))
}

