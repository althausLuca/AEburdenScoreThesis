# #' @title Model Settings
# #' @description This file contains the settings for the models used in the analysis.
# #' Fixed names (latex representation), colors , lines styled and markers are defined here for plotting.
# source("R/helpers.R")
#
# #' @title Pre-process Model Name for Consistency
# pre_process_name <- function(model_name) {
#   if (grepl("tweedie", model_name)) {
#     model_name <- "tweedie"
#   }
#   if (grepl("permutation", model_name)) {
#     model_name <- "permutation_test"
#   }
#   if (grepl("zero_inflated_ttest", model_name) | grepl("two_part_ttest", model_name)) {
#     model_name <- "two_part_t_test"
#   }
#   if (grepl("quantile_regression_tau_0.75", model_name)) {
#     model_name <- "quantile_regression_tau_75"
#   }
#   if (grepl("quantile_regression_tau_0.5", model_name)) {
#     model_name <- "quantile_regression_tau_0.5"
#   }
#
#   return(model_name)
# }
#
#
# get_color <- function(model_name) {
#   model_name <- pre_process_name(model_name)
#   color <- switch(model_name,
#                   log_anova = "navy",
#                   anova = "purple",
#                   tweedie = "tomato4",
#                   quantile_regression_tau_0.5 = "lightpink4",
#                   quantile_regression_tau_75 = "lightpink1",
#                   permutation_test = "yellow3",
#                   wilcoxon = "darkgreen",
#
#                   ## log anova models
#                   log_anova_c_0.001 = "royalblue4",
#                   log_anova_c_1 = "navy",
#                   log_anova_c_10000 = "royalblue1",
#
#                   ## Zero inflated models
#                   two_part_t_test = "red",
#                   two_part_wilcoxon = "orangered1",
#                   zero_inflated_gamma = "red4",
#                   zero_inflated_lognormal = "maroon1",
#                   "black"
#   )
#
#   return(unname(color))
# }
#
#
# get_marker <- function(model_name) {
#   model_name <- pre_process_name(model_name)
#   shape <- switch(model_name,
#                   log_anova = 1,
#                   anova = 5,
#                   tweedie = 4,
#                   quantile_regression = 3,
#                   quantile_regression_tau_0.5 = 3,
#                   quantile_regression_tau_75 = 1,
#                   permutation_test = 5,
#                   wilcoxon = 6,
#                   log_anova_c_0.001 = 1,
#                   log_anova_c_1 = 2,
#                   log_anova_c_10000 = 3,
#
#                   ## Zero inflated models
#                   two_part_t_test = 4,
#                   zero_inflated_gamma = 5,
#                   zero_inflated_lognormal = 6,
#                   two_part_wilcoxon = 1,
#                   # default case
#                   1
#
#   )
#
#   return(unname(shape))
# }
#
#
# get_line_style <- function(model_name) {
#   model_name <- pre_process_name(model_name)
#   line_style <- switch(model_name,
#                        log_anova = "dotdash",
#                        anova = "dashed",
#                        tweedie = "dotted",
#                        quantile_regression = "solid",
#                        quantile_regression_tau_0.5 = "solid",
#                        quantile_regression_tau_75 = "dotted",
#                        permutation_test = "dashed",
#                        wilcoxon = "dashed",
#                        log_anova_c_0.001 = "dotdash",
#                        log_anova_c_1 = "dotted",
#                        log_anova_c_10000 = "1F",
#
#                        ## Zero inflated models
#                        two_part_t_test = "dotted",
#                        zero_inflated_gamma = "dotdash",
#                        zero_inflated_lognormal = "dashed",
#                        two_part_wilcoxon = "1F",
#                        # default case
#                        "solid"
#   )
#
#   return(unname(line_style))
# }
#
#
#
# map_labels <- function(model_names) {
#   #check if it is a string
#   if (length(model_names)==1) {
#         return(map_label(model_names))
#   }
#   else{
#     for(i in 1:length(model_names)){
#       model_names[[i]] <- map_label(model_names[i])
#     }
#   }
#   return(model_names)
# }
#
# # Return Label for latex expressions using library(latex2exp)
# map_label <- function(model_name) {
#   if (grepl("tau_0.5", model_name)) {
#     return("Median Regression")
#   }
#
#   else if (grepl("tau", model_name)) {
#     tau <- strsplit(model_name, "_")[[1]][4]
#     return(paste0("Quantile Regression $$$_{\\tau=", tau, "}$"))
#   }
#
#   model_name <- pre_process_name(model_name)
#   #tau_0.5 in
#   model_labels <- c(
#     anova = "ANOVA",
#     tweedie = "Tweedie Regression",
#     quantile_regression = "Quantile Regression",
#     log_anova_c_0.001 = "Log-ANOVA $$$_{c=0.001}$",
#     log_anova_c_1 = "Log-ANOVA $$$_{c=1}$",
#     log_anova_c_10000 = "Log-ANOVA $$$_{c=10000}$",
#     permutation_test = "Permutation Test",
#     wilcoxon = "Wilcoxon Test",
#     zero_inflated_gamma = "Zero-Inflated Gamma",
#     log_anova = "Log-ANOVA",
#     two_part_t_test = "Two-Part T-Test",
#     zero_inflated_lognormal = "Zero-Inflated Lognormal",
#     two_part_wilcoxon = "Two-Part Wilcoxon Test",
#     zero_inflated_normal = "Zero-Inflated Normal"
#   )
#   if (model_name %in% names(model_labels)) {
#     return(model_labels[[model_name]])
#   } else {
#     return(model_name)
#   }
# }
#
# order_models <- function(model_reprs) {
#   model_reprs <- sort(model_reprs)
#   processed_names <- sapply(model_reprs, pre_process_name)
#
#   models_with_anova <- grepl("anova", processed_names)
#   qr_models <- grepl("quantile_regression", processed_names)
#   tweedie_models <- grepl("tweedie", processed_names)
#   zero_inflated_models <- grepl("zero_inflate", processed_names)
#   two_part_tests <- grepl("two_part", processed_names)
#
#   others <- !models_with_anova &
#     !zero_inflated_models &
#     !two_part_tests &
#     !qr_models &
#     !tweedie_models
#
#   result <- c(model_reprs[models_with_anova],
#               model_reprs[qr_models],
#               model_reprs[tweedie_models],
#               model_reprs[zero_inflated_models],
#               model_reprs[others],
#               model_reprs[two_part_tests])
#   if (length(result) != length(model_reprs)) {
#     stop("Error in ordering models")
#   }
#   return(result)
# }
#
#
#
#
#
#
#
#
