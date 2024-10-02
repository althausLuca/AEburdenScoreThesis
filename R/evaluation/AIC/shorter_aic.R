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
                       "zero_inflated_ttest","log_anova_c_10000",
                        "log_anova_c_0.001", "log_anova_c_1" ,  "log_anova_c_10000")

model_files <- list.files(model_folder, full.names = TRUE)

#files with _qr
model_files <- model_files[!grepl("_qr", model_files)]
store_name <- "shorter_aic.R"

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

colnames(AIC_values)
tweedie_AIC <- AIC_values[,"tweedie_var_power_1.2_link_power_0"]
zero_inflade_lognormal_AIC <- AIC_values[,"zero_inflated_lognormal"]
tweedie_AIC- zero_inflade_lognormal_AIC
median(relative_likelyhood(tweedie_AIC, zero_inflade_lognormal_AIC))

zero_inflated_gamma_AIC <- AIC_values[,"zero_inflated_gamma"]
median(relative_likelyhood(zero_inflated_gamma_AIC, zero_inflade_lognormal_AIC))

anova_AIC <- AIC_values[,"anova"]
median(relative_likelyhood(anova_AIC, tweedie_AIC))


