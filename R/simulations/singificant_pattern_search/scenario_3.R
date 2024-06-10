source("R/methods/run_methods.R")
source("R/helpers.R")

trials_folder <- "data/trials"
model_folder <- "data/model_results"

trial_files <- list.files(trials_folder, full.names = FALSE)
model_files <- list.files(model_folder, full.names = FALSE)


results <- data.frame()
for (model_file in model_files) {
  #check if _l_ is in model_file
  if(! grepl(  "_l_", model_file, fixed = TRUE)){
    next
  }
  if(! 1.5 == get_prefixed_number(model_file,"_k_")){
    next
  }
  l_ <- get_prefixed_number(model_file,"_l_")
  print(l_)
  p_values <- get_values(model_file, "p_value")
  sig_p_values <- colMeans(p_values < 0.05)
  results <- rbind(results, cbind(l_ = l_, t(sig_p_values)))
}
results <- results[order(results[,1]),]
#plot
library(ggplot2)
library(dplyr)
library(tidyr)

# Convert the data to a long format suitable for plotting with ggplot
results_long <- results %>%
  pivot_longer(-names(results)[1], names_to = "model", values_to = "value")

results_long$model[results_long$model == "anova"] <- "anova_c_1"

# Generate a color palette
colors <- grDevices::rainbow(length(unique(results_long$model)))

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

    # Add other model replacements here
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
g <- ggplot(results_long, aes(x = l_, y = value, group=model)) +
  geom_line(aes(color = model, linetype = model),size=1.1) + # Map both color and linetype to model
  labs(x = "Longer Factor", y = "Proportion of Significant P-values", title = "", color = "Model", linetype = "Model") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 15) , legend.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 14 , face="bold"), axis.title  = element_text(size = 17, face="bold")) +
  theme(legend.position = c(0.02, 0.95), legend.justification = c("left", "top")) +
  theme(legend.key.width = unit(0.9, "cm")) +
  ylim(0, 1)

g  <- g +  scale_color_manual(values = colors,
                              labels = lapply(sort(unique(results_long$model)),  function(x) TeX(map_labels(x)))) +
  scale_linetype_manual(values = setNames(1:length(unique(results_long$model)), unique(results_long$model)),
                        labels = lapply(sort(unique(results_long$model)), function(x) TeX(map_labels(x))))+
  guides(fill=guide_legend(override.aes=list(size=15)))
g
ggsave("longer_p_values.pdf", plot = g, width = 10, height = 5)



source("R/trials/trial_simulation.R")
source("R/Scenarios.R")
source("R/trials/analysis/plots.R")


n_sim <- 5000
n_subjects <- 100 # per group
scenario_name <- "Scenario_3"
longer <- 3.5
k <- 1.5

scenario <- load_scenario(scenario_name, longer = longer)


dev.off()

control.group <- simulate_group(scenario$control, size = n_sim, susceptibility_parameter = list("gamma", k), max_time = 180)
treatment.group <- simulate_group(scenario$treatment, size = n_sim, susceptibility_parameter = list("gamma", k), max_time = 180)



hist_ylim <- 240
box_lim <- 800


hist_and_box_plot(treatment.group$scores, treatment.group$n_events
  , file_name = paste0("pres_scenario_3_treatment_l_", longer, ".pdf") , main="Experimental Group"
  ,hist_lim = 100 , box_lim = box_lim <- 800
  , hist_y_lim = hist_ylim , bin_width = 1)
hist_and_box_plot(control.group$scores, control.group$n_events , box_lim = box_lim <- 800
  , hist_y_lim = hist_ylim
  , file_name = paste0("pres_scenario_3_control_sl_", longer, ".pdf")
  , hist_lim = 100 , main="Control Group", bin_width = 1)
