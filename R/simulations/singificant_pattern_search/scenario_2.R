library(ggplot2)
library(dplyr)
library(tidyr)

source("R/models/run_models.R")

trials_folder <- "../../../data_old/trials"
model_folder <- "data/model_results_"


trial_files <- list.files(trials_folder, full.names = FALSE)
model_fildes <- list.files(model_folder, full.names = FALSE)


results <- data.frame()
for (model_file in model_fildes) {
  s_ <- as.numeric(sub(".*_s_([0-9]+(?:\\.[0-9]+)?).*", "\\1", model_file))
  if (is.na(s_)) {
    print("shorter factor is NaN")
    next
  }
  p_values <- get_values(model_file, "p_value")
  sig_p_values <- colMeans(p_values < 0.05)
  if (length(names((sig_p_values))) + 1 == ncol(results) || ncol(results) == 0) {
    results <- rbind(results, cbind(s_ = s_, t(sig_p_values)))
  }
}

# Convert the data to a long format suitable for plotting with ggplot
results_long <- results %>%
  pivot_longer(-names(results)[1], names_to = "model", values_to = "value")

results_long$model[results_long$model == "lm"] <- "ANOVA"
results_long  <- results_long[results_long$model != "anova_c_100000000",]
library(latex2exp) #TeX(sprintf("$\\xi = %.2f$", xi))))

map_labels <- function(x) {
  model_labels <- c(
    ANOVA = "$ANOVA$",
    tweedie = "Tweedie Regression",
    quantile_regression = "Median Regression",
    anova_c_0.001 = "Log-ANOVA $$$_{c=0.001}$",
    anova_c_1 = "Log-ANOVA $$$_{c=1}$",
    anova_c_10000 =  "Log-ANOVA $$$_{c=10000}$",
    anova_c_100000000 ="Log-ANOVA $$$_{c=100000000}$"
  )

  if (x %in% names(model_labels)) {
    return(model_labels[[x]])
  } else {
    return(x)
  }
}

# Generate a color palette
colors <- c("green", "navy", "royalblue4", "royalblue2", "black", "orange")

# Plot with line styles
# Significant P-values For different Factors for Shorter Event Gap Times
g <- ggplot(results_long, aes(x = s_, y = value, group=model)) +
  geom_line(aes(color = model, linetype = model),size=1.1) + # Map both color and linetype to model
  labs(x = "Shorter Factor", y = "Proportion of Significant P-values", title = "", color = "Model", linetype = "Model") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 15) , legend.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 14 , face="bold"), axis.title  = element_text(size = 17, face="bold")) +
  theme(legend.position = c(0.95, 0.95), legend.justification = c("right", "top")) +
  theme(legend.key.width = unit(1, "cm")) +
  ylim(0, 1)

g  <- g +  scale_color_manual(values = colors,
labels = lapply(sort(unique(results_long$model)),  function(x) TeX(map_labels(x)))) +
scale_linetype_manual(values = setNames(1:length(unique(results_long$model)), unique(results_long$model)),
labels = lapply(sort(unique(results_long$model)), function(x) TeX(map_labels(x))))+
  guides(fill=guide_legend(override.aes=list(size=15)))
g
ggsave("shorter_p_values.pdf", plot = g, width = 10, height = 5)

