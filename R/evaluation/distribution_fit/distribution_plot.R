library(ggplot2)
library(grid)
library(reshape2)

distributions_plot <- function(distributions, x, Y ,  distributions_title = "Distributions", ecdf_title = "ECDF" , max_n_trials = Inf , text_size_factor = 1) {
  if(!is.matrix(distributions)){
    distribution_matrix <- do.call(cbind, distributions)
  } else {
    distribution_matrix <- distributions
  }

  if(nrow(distribution_matrix) != length(x)){
    distribution_matrix <- t(distribution_matrix)
    if(nrow(distribution_matrix) != length(x)){
      stop("The number of rows in the distribution matrix must be equal to the length of x")
    }
  }

  distribution_matrix <- distribution_matrix[,1:min(ncol(distribution_matrix), max_n_trials)]

  distribution_df <- as.data.frame(distribution_matrix)
  colnames(distribution_df) <- paste("dist", 1:ncol(distribution_df), sep = "_")
  distribution_df$x <- x
  melted_df <- melt(distribution_df, id.vars = "x")
  colnames(melted_df) <- c("x", "trial", "value")
  melted_df$line_type <- "Distributions"
  ecdf_data <- data.frame(x = x, value = ecdf(Y)(x))
  ecdf_data$line_type <- "ECDF"
  ecdf_data$trial <- "_"

  combined_data <- rbind(melted_df, ecdf_data)

  p <- ggplot(combined_data, aes(x = x, y = value, linewidth  = line_type, color = line_type, group = interaction(trial, line_type))) +
    geom_line(data = subset(combined_data, line_type == "Distributions" & x > 0), alpha = 0.03 ,linewidth =0.12 ) +
    geom_line(data = subset(combined_data, line_type == "Distributions" & x <= 0), alpha = 0.03 ,linewidth =0.12 ) +
    geom_line(data = subset(combined_data, line_type == "ECDF" & x > 0), linewidth = 0.3) +
    geom_line(data = subset(combined_data, line_type == "ECDF" & x <= 0), linewidth = 0.3) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    labs(x = "Score", y = "F(x)", color="")

  p <- p + scale_color_manual(values = c("Distributions" = "blue", "ECDF" = "red"),
                              labels = c("Distributions" = distributions_title, "ECDF" = ecdf_title)) +
    theme(plot.title = element_text(vjust = 0) , legend.text = element_text(size = round(17*text_size_factor)) ,
          legend.title = element_text(size = 0), legend.spacing.x = unit(20,"cm"),
          legend.position = "top" ,legend.box.margin = margin(-5, -10, -10, -10))+
    guides(color = guide_legend(override.aes = list(linewidth  = 1, alpha = c(1, 1) ,size=3)))+
    theme(axis.text = element_text(size = round(15*text_size_factor) , face="bold"), axis.title  = element_text(size = round(17*text_size_factor), face="bold"))

  return(p)
}