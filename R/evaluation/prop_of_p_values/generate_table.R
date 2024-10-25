source("R/models_and_tests/model_computer.R")


file.1 <- "results_24_10/Scenario_1_k_1.5.RData"
file.2 <- "results_24_10/sample_size_variation/equal_50.RData"
file.3 <- "results_24_10/sample_size_variation/equal_20.RData"

files <- c("100" = file.1 , "50" = file.2, "20" = file.2)
# file <-  list.files(folder, full.names = TRUE)[[7]]
# df <- NULL

# colnames(result_df)

model_map <- list(
  wilcoxon = "Wilcoxon",
  permutation_test_nper_10000 = "Permutation Test",
  anova = "ANOVA",
  log_anova_c_0.001 = "Log-ANOVA $_{c=0.001}$",
  log_anova_c_1 = "Log-ANOVA $_{c=1}$",
  log_anova_c_10000 = "Log-ANOVA $_{c=10000}$",
  tweedie_var_power_infer_link_power_0 = "Tweedie Regression",
  tweedie_var_power_1.2_link_power_0 = "Tweedie Regression $_{xi=1.2}$",
  tweedie_var_power_1.8_link_power_0 = "Tweedie Regression $_{xi=1.8}$",
  zero_inflated_normal = "Zero-Inflated Normal",
  zero_inflated_gamma = "Zero-Inflated Gamma",
  zero_inflated_lognormal = "Zero-Inflated Lognormal",
  zero_inflated_lognormal_per_group = "Zero-Inflated Lognormal^*",
  zero_inflated_gamma_sigma_per_group = "Zero-Inflated Gamma^*",
  two_part_wilcoxon = "Two-Part Wilcoxon Test",
  two_part_ttest = "Two-Part T-Test",
  two_part_welch = "Two-Part Welch Test",
  quantile_regression_tau_0.5_xy = "Median Regression",
  quantile_regression_tau_0.75_xy = "Quantile Regression $_{\\tau=0.75}$"
)

alpha_values <- c(0.01, 0.05, 0.1)

results <- list()
for (i in seq_along(files)) {
  name <- names(files)[[i]]
  file <- files[[i]]
  model_computer <- load_model_computer(file)
  values <- get_value(model_computer, "p_value")

  result_df <- NULL
  for (alpha in alpha_values) {
    result_df <- rbind(result_df, colMeans(values < alpha))
  }
  results[[name]] <- result_df
}

n <- length(results)


cat("%" ,  names(results)  , "\n \\hline \n", file = "output.txt")
cat("model  & ", paste(rep(alpha_values,n), collapse = " & "), "\\\\ \n", file = "output.txt", append = TRUE)
for (model_rep in names(model_map)) {
  if (model_rep %in% colnames(result_df)) {
    model_name <- unlist(unname(model_map[model_rep]))
  } else next
  line <- NULL
  for(result_df in results){
    new_line <- paste( round(result_df[, model_rep], 3), collapse = " & " )
    new_line <- gsub("0\\." , "." , new_line)
    line <- paste0(line, " & ",  new_line , sep = "" )
  }
  cat(model_name, line, "\\\\ \n", file = "output.txt", append = TRUE)
}

