aic_model_map <- list(
  anova = "ANOVA",
  tweedie_var_power_infer_link_power_0 = "Tweedie Regression",
  # tweedie_var_power_1.2_link_power_0 = "Tweedie Regression $_{xi=1.2}$",
  # tweedie_var_power_1.8_link_power_0 = "Tweedie Regression $_{xi=1.8}$",
  tweedie_var_power_infer_link_power_0_mle = "Tweedie Regression $_{\\hat{\\phi}^{MLE}}$",
  # tweedie_var_power_1.2_link_power_0_mle = "Tweedie Regression $_{{xi=1.2} \\, \\hat{\\phi}^{MLE} }$",
  # tweedie_var_power_1.8_link_power_0_mle = "Tweedie Regression $_{{xi=1.8} \\, \\hat{\\phi}^{MLE} }$",
  zero_inflated_gamma = "Zero-Inflated Gamma",
  zero_inflated_gamma_sigma_per_group = "Zero-Inflated Gamma$^*$",
  zero_inflated_lognormal = "Zero-Inflated Lognormal",
  zero_inflated_lognormal_per_group = "Zero-Inflated Lognormal$^*$",
  zero_inflated_normal_per_group = "Zero-Inflated Normal",
  zero_inflated_normal = "Zero-Inflated Normal$_{\\sigma^+_C=\\sigma^+_E}$"
)

aic_full_model_map <- list(
  anova = "ANOVA",
  log_anova_c_0.001 = "Log-ANOVA $_{c=0.001}$",
  log_anova_c_1 = "Log-ANOVA $_{c=1}$",
  log_anova_c_10000 = "Log-ANOVA $_{c=10000}$",
  tweedie_var_power_infer_link_power_0 = "Tweedie Regression",
  tweedie_var_power_1.2_link_power_0 = "Tweedie Regression $_{xi=1.2}$",
  tweedie_var_power_1.8_link_power_0 = "Tweedie Regression $_{xi=1.8}$",
  tweedie_var_power_infer_link_power_0_mle = "Tweedie Regression $_{\\hat{\\phi}^{MLE}}$",
  # tweedie_var_power_1.2_link_power_0_mle = "Tweedie Regression $_{{xi=1.2} \\, \\hat{\\phi}^{MLE} }$",
  # tweedie_var_power_1.8_link_power_0_mle = "Tweedie Regression $_{{xi=1.8} \\, \\hat{\\phi}^{MLE} }$",
  # zero_inflated_normal = "Zero-Inflated Normal",
  zero_inflated_gamma = "Zero-Inflated Gamma",
  zero_inflated_lognormal = "Zero-Inflated Lognormal$_{\\sigma_C=\\sigma_E}$",
  zero_inflated_lognormal_per_group = "Zero-Inflated Lognormal$^*$",
  zero_inflated_gamma_sigma_per_group = "Zero-Inflated Gamma$^*$",
  quantile_regression_tau_0.5_xy = "Median Regression",
  quantile_regression_tau_0.75_xy = "Quantile Regression $_{\\tau=0.75}$"
)

