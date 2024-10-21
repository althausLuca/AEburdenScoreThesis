library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)
# source("R/simulations/default_models.R")
source("R/models_and_tests/model_computer.R")
source("R/helpers.R")


model_folder <- "results/shorter_gap_times"
store_name <- "shorter_p_values.pdf"

models_to_exclude <- c("tweedie_var_power_1.5_link_power_0",
                       "zero_inflate_wilcoxon",
                       "quantile_regression_tau_0.5",
                       "zero_inflated_ttest",
                        "log_anova_c_0.001", "log_anova_c_1" ,  "log_anova_c_10000",
                       "quantile_regression_tau_0.75_xy" ,
                       "quantile_regression_tau_0.5_xy",
                       "tweedie_var_power_1.8_link_power_0",
                       "tweedie_var_power_1.2_link_power_0"

)

model_files <- list.files(model_folder, full.names = TRUE)

#files with _qr
model_files <- model_files[!grepl("_qr", model_files)]

model_file_name <- "results/shorter_gap_times/Scenario_2_k_1.5_s_1.RData"
model_computer <- load_model_computer(model_file_name)

AIC_values_full <-  as.data.frame(get_value(model_computer, "AIC"))
class(AIC_values_full)

AIC_values <- AIC_values_full[,!(colnames(AIC_values_full) %in% models_to_exclude)]

aic_order <- t(apply(AIC_values, 1,function(row) colnames(AIC_values)[order(row)]))


for(i in 1:ncol(aic_order)[1]){
  print(i)
  print(table(aic_order[,i]))
}

relative_likelyhood <- function(aic_model1, aic_model2){
    return(exp((aic_model1 - aic_model2)/2))
}
#
# colnames(AIC_values)
# tweedie_AIC <- AIC_values[,"tweedie_var_power_1.2_link_power_0"]
# zero_inflade_lognormal_AIC <- AIC_values[,"zero_inflated_lognormal"]
# tweedie_AIC- zero_inflade_lognormal_AIC
# median(relative_likelyhood(tweedie_AIC, zero_inflade_lognormal_AIC))
#
# zero_inflated_gamma_AIC <- AIC_values[,"zero_inflated_gamma"]
# median(relative_likelyhood(zero_inflated_gamma_AIC, zero_inflade_lognormal_AIC))
#
# anova_AIC <- AIC_values[,"anova"]
# median(relative_likelyhood(anova_AIC, tweedie_AIC))
#
#
# median(relative_likelyhood(anova_AIC, zero_inflade_lognormal_AIC))
#

# plot(AIC_values[,"tweedie_var_power_1.2_link_power_0"], AIC_values[,"zero_inflated_lognormal"])

# transform df to long format
AIC_df_long <- AIC_values %>%
  mutate(trial = rownames(.)) %>%
  gather(key = "model", value = "AIC", -trial)
head(AIC_df_long)

source("R/models_and_tests/model_settings.R")

model_names <- colnames(AIC_values)
model_names <- rev(order_models(model_names))
model_labels <- map_labels(model_names)

AIC_df_long$model <- factor(AIC_df_long$model, levels = model_names)
g <- ggplot(AIC_df_long, aes(x=model, y=AIC)) +
  geom_boxplot() + coord_flip() +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed") +  # modify x axis labels
  scale_x_discrete(labels = model_labels , breaks = model_names)

#increase axis labels
g <- g +  theme(axis.text.y = element_text(size = 14),
                axis.text.x = element_text(size = 14),
                axis.title.y = element_text(size = 0),
                axis.title.x = element_text(size = 15),
                )
g
colMeans(AIC_values_full)

path <- "plots/AIC/"

#save g to pdf
ggsave(paste0(path, store_name), plot = g, device = "pdf", width = 20, height = 10, units = "cm")