model_cdf_plots <- function(model, trial_data,
                            scenario_name = "",
                            plot_path = "plots/model_distributions/") {

  if (scenario_name != "") {
    plot_path <- paste0(plot_path, scenario_name, "/")
  }

  source("R/evaluation/distribution_fit/x.R")
  source("R/evaluation/analysis_and_comparison/distribution_plot.R")


  CDFS <- trial_data$apply_to_each(function(trial) fit_model(model, trial)$get_CDFs(x))

  control_densities <- lapply(CDFS, function(CDF) CDF$control)
  treatment_densities <- lapply(CDFS, function(CDF) CDF$treatment)

  Y_control <- trial_data$all_data()[trial_data$all_data()$Group == "control", 1]
  Y_treatment <- trial_data$all_data()[trial_data$all_data()$Group == "treatment", 1]

  dir.create(plot_path, recursive = TRUE, showWarnings = FALSE)

  p_control <- distributions_plot(control_densities, x, Y_control, paste0(model$name, " Distributions"))
  ggsave(paste0(plot_path, model$repr, "_control.pdf"), plot = p_control, width = 8, height = 5)

  p_treatment <- distributions_plot(treatment_densities, x, Y_treatment, paste0(model$name, " Distributions"))

  ggsave(paste0(plot_path, model$repr, "_treatment.pdf"), plot = p_treatment, width = 8, height = 5)


  return(list(control = p_control, treatment = p_treatment))
}