library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp) #TeX(sprintf("$\\xi = %.2f$", xi))))


source("R/methods/run_methods.R")
source("R/methods/methods_settings.R")

model_file <- "Scenario_2_k_1.5_s_0.5.csv"
filename <- "p_values_qq_shorter.pdf"
#
# model_file <- "Scenario_1_k_1.5.csv"
# filename <- "p_values_qq_no_diff.pdf"
#
model_file <- "Scenario_3_k_1.5_l_3.5.csv"
filename <- "p_values_qq_longer.pdf"



p_values <- get_values(model_file, "p_value")
sig_p_values <- colMeans(p_values < 0.05)
sig_p_values

#change lm to ANOVA
sig_p_values

data <- data.frame(
  setNames(
    list(
      sort(p_values[[LM]]),
      sort(p_values[["anova_c_0.001"]]),
      sort(p_values[["anova_c_1"]]),
      sort(p_values[[TWEEDIE]]),
      sort(p_values[[QUANTILE_REGRESSION]])
    ),
    c("ANOVA", "anova_c_0.001", "anova_c_1","tweedie", "quantile_regression")
  )
) %>% mutate(id = row_number())%>%
  pivot_longer(cols = -id, names_to = "model", values_to = "p_value")

rescale_x <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
data <- data %>% mutate(rescaled_id = rescale_x(id))


colors <- c("green", "navy", "royalblue4", "black", "orange")

map_labels <- function(x) {
  model_labels <- c(
    ANOVA = "$ANOVA$",
    tweedie = "Tweedie Regression",
    quantile_regression = "Median Regression",
    anova_c_0.001 = "Log-ANOVA $$$_{c=0.001}$",
    anova_c_1 = "Log-ANOVA $$$_{c=1}$"
  )
  if (x %in% names(model_labels)) {
    return(model_labels[[x]])
  } else {
    return(x)
  }
}
rescale_x <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

g <- ggplot(data, aes(x = p_value, y = rescaled_id, group=model)) +
  geom_line(aes(color = model, linetype = model),size=1.1) + # Map both color and linetype to model
  labs(x = "P-Value", y = "CDF", title = "") +
geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(
    legend.position = c(0.99, 0.01), # Top left corner inside the plot
    legend.justification = c(1, -0.1),  # Anchor point of the legend box
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    legend.box.just = "left",
    axis.text = element_text(size = 13 , face="bold"),
    axis.title  = element_text(size = 16, face="bold")
  )
#g
g  <- g +  scale_color_manual(values = colors,
                              labels = lapply(sort(unique(data$model)),  function(x) TeX(map_labels(x)))) +
  scale_linetype_manual(values = setNames(1:length(unique(data$model)), unique(data$model)),
                        labels = lapply(sort(unique(data$model)), function(x) TeX(map_labels(x))))+
  guides(fill=guide_legend(override.aes=list(size=15))) +
  (vl <- geom_vline(xintercept = 0.05, linetype = "dashed", color = "red")) +
  annotate("text", x=0.1, y=-0, label="0.05", angle=0,color = "red")
g



ggsave(filename, plot = g, width = 6, height = 6)
