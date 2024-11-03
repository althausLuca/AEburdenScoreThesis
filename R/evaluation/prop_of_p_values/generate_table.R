source("R/evaluation/config.R", local = (config <- new.env()))

alpha_values <- c(0.01, 0.05, 0.1)

table_path <- "tables/sample_size_variation/"
dir.create(table_path, recursive = TRUE, showWarnings = FALSE)

scenarios <- list(
  equal = c("100" = "results/s1_k_1.5.RData",
            "50" = "results/sample_size_variation/equal_50.RData",
            "20" = "results/sample_size_variation/equal_20.RData"
  ),
  shorter = c("100" = config$DEFAULT_GAP_TIME_VAR_FILE,
              "50" = "results/sample_size_variation/shorter_50.RData",
              "20" = "results/sample_size_variation/shorter_20.RData"
  ),
  longer = c("100" = config$DEFAULT_DURATION_VAR_FILE,
             "50" = "results/sample_size_variation/longer_50.RData",
             "20" = "results/sample_size_variation/longer_20.RData"
  )
)




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
  tweedie_var_power_infer_link_power_0_mle = "Tweedie Regression $_{\\hat{\\phi}^{MLE}}$",
  tweedie_var_power_1.2_link_power_0_mle = "Tweedie Regression $_{{xi=1.2} \\, \\hat{\\phi}^{MLE} }$",
  tweedie_var_power_1.8_link_power_0_mle = "Tweedie Regression $_{{xi=1.8} \\, \\hat{\\phi}^{MLE} }$",
  zero_inflated_normal = "Zero-Inflated Normal",
  zero_inflated_gamma = "Zero-Inflated Gamma",
  zero_inflated_gamma_sigma_per_group = "Zero-Inflated Gamma$^*$",
  zero_inflated_lognormal = "Zero-Inflated Lognormal",
  zero_inflated_lognormal_per_group = "Zero-Inflated Lognormal$^*$",
  two_part_wilcoxon = "Two-Part Wilcoxon Test",
  two_part_welch = "Two-Part Welch Test",
  two_part_ttest = "Two-Part T-Test",
  quantile_regression_tau_0.5_xy = "Median Regression",
  quantile_regression_tau_0.75_xy = "Quantile Regression $_{\\tau=0.75}$"
)


for (s in seq_along(scenarios)) {
  files <- scenarios[[s]]
  name <- names(scenarios)[[s]]
  file_path <- paste0(table_path, name, ".txt")

  results <- list()
  for (i in seq_along(files)) {
    name <- names(files)[[i]]
    file <- files[[i]]
    model_computer <- load_model_computer(file)
    values <- get_value(model_computer, "p_value")

    result_df <- NULL
    for (alpha in alpha_values) {
      result_df <- rbind(result_df, colMeans(values < alpha, na.rm = TRUE)) # we got a NA value once
    }
    results[[name]] <- result_df
  }

  n <- length(results)
  print(results)

  cat("%", names(results), "\n \\hline \n", file = file_path)
  cat("Model  & ", paste(rep(alpha_values, n), collapse = " & "), "\\\\ \n", file = file_path, append = TRUE)
  for (model_rep in names(model_map)) {
    if (model_rep %in% colnames(result_df)) {
      model_name <- unlist(unname(model_map[model_rep]))
    } else next
    line <- NULL
    for (result_df in results) {
      new_line <- paste(round(result_df[, model_rep], 3), collapse = " & ")
      new_line <- gsub("0\\.", ".", new_line)
      line <- paste0(line, " & ", new_line, sep = "")
    }
    cat(model_name, line, "\\\\ \n", file = file_path, append = TRUE)
  }
}


