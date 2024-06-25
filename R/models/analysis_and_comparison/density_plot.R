library(ggplot2)
library(grid)
library(reshape2)

density_plot <- function(distributions, x , Y , distributions_title = "Distributions" , ecdf_title = "ECDF") {
  distribution_matrix <- do.call(cbind, distributions)
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

  p <- ggplot(combined_data, aes(x = x, y = value, size = line_type, color = line_type, group = interaction(trial, line_type))) +
    geom_line(data = subset(combined_data, line_type == "Distributions" & x > 0), alpha = 0.05 ,size=0.5 ) +
    geom_line(data = subset(combined_data, line_type == "Distributions" & x < 0), alpha = 0.05 ,size=0.5 ) +
    geom_line(data = subset(combined_data, line_type == "ECDF" & x > 0), size= 0.8) +
    geom_line(data = subset(combined_data, line_type == "ECDF" & x < 0), size= 0.8) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    labs(x = "Score", y = "F(x)", color="")
  p <- p + scale_color_manual(values = c("Distributions" = "blue", "ECDF" = "red"),
                              labels = c("Distributions" = distributions_title, "ECDF" = ecdf_title)) +
    theme(plot.title = element_text(vjust = 0) , legend.text = element_text(size = 17) ,
          legend.title = element_text(size = 0), legend.spacing.x = unit(20,"cm"),
          legend.position = "top" ,legend.box.margin = margin(-5, -10, -10, -10))+
    guides(color = guide_legend(override.aes = list(linewidth  = 1, alpha = c(1, 1) ,size=3)))+
    theme(axis.text = element_text(size = 15 , face="bold"), axis.title  = element_text(size = 17, face="bold"))

  p

  return(p)
}