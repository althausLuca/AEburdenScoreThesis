library(ggplot2)
library(dplyr)
library(tidyr)

source("R/methods/run_methods.R")
source("R/methods/methods_settings.R")

model_file <- "Scenario_2_k_1.5_s_0.5.csv"

p_values <- get_values(model_file, "p_value")
sig_p_values <- colMeans(p_values < 0.05)
sig_p_values

AIC <- get_values(model_file, "AIC")



data <- data.frame(
  setNames(
    list(
      sort(p_values[[TWEEDIE]]),
      sort(p_values[[LOG_ANOVA]]),
      sort(p_values[[LM]]),
      sort(p_values[[QUANTILE_REGRESSION]])
    ),
    c(TWEEDIE, LOG_ANOVA, LM, QUANTILE_REGRESSION)
  )
) %>% mutate(id = row_number())%>%
  pivot_longer(cols = -id, names_to = "method", values_to = "p_value")

colors <- MODEL_COLORS
linetypes <-MODEL_LINETYPES

ggplot(data_long, aes(x = id, y = p_value, color = method, linetype = method)) +
  geom_line() +
  scale_color_manual(values = colors) +
  scale_linetype_manual(values = linetypes) +
  labs(x = "Rank", y = "P-value", title = "P-values Comparison") +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  theme_minimal() +
  theme(
    legend.position = c(0.01, 0.99), # Top left corner inside the plot
    legend.justification = c(0, 1),  # Anchor point of the legend box
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    legend.box.just = "left"
  )



