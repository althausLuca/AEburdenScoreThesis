# name_map <- c("anova" = "ANOVA",
#               "log_anova_c_0.001" = "Log-ANOVA$_{c=0.001}$",
#               "log_anova_c_1" = "Log-ANOVA$_{c=1}$",
#               tweedie_var_power_1.2_link_power_0 = "Tweedie",
#               "wilcoxon" = "Wilcoxon",
#               "permutation_test_nper_10000" = "Perm. Test",
#               "quantile_regression_tau_0.5_xy" = "Median Reg.",
#               "quantile_regression_tau_0.75_xy" = "Quant. reg. $tau=0.75$",
#               zero_inflated_gamma = "ZIG",
#               zero_inflated_lognormal = "ZILN",
#               two_part_ttest = "Two-Part T-Test",
#               two_part_wilcoxon = "Two-Part Wilcoxon")
#
#
# long_df_to_table <- function(df) {
#   models_names <- unique(df$model)
#   row_names <- unique(df$scenario_factor)
#
#   data_frame <- data.frame(matrix(ncol = length(models_names), nrow = length(row_names)))
#   colnames(data_frame) <- models_names
#   rownames(data_frame) <- row_names
#
#   for (row in row_names) {
#     for (model in models_names) {
#       value <- df$value[df$scenario_factor == row & df$model == model]
#       if (length(value) == 0) {
#         value <- NA
#       }
#       data_frame[row, model] <- round(value, 4)
#     }
#   }
#   colnames(data_frame) <- name_map[colnames(data_frame)]
#
#   data_frame <- t(data_frame)
#   for (j in 1:nrow(data_frame)) {
#     for (i in 1:(ncol(data_frame) - 1)) {
#       data_frame[j, i] <- paste0(as.character(data_frame[j, i]), " & ")
#     }
#     data_frame[j, i+1] <- paste0(as.character(data_frame[j, i+1]),"\\")
#   }
#
#   return(data_frame)
# }