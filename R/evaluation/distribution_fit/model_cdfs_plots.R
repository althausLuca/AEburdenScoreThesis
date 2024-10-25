get_x_range <- function(model){

  pos_log <- c(0,pracma::logseq(0.1, 600, n = 90))
  default <- c(-30, pos_log)
  repr <- model$repr

  if(LOG_ANOVA(c = 0.001)$repr == repr){
    return(c(-30, -0.001, seq( -0.001, 0, length.out = 5), pos_log))
  }
  if(LOG_ANOVA(c = 1)$repr == repr){
    return(c(-30, -1, seq( -1, 0, length.out = 5), pos_log))
  }
  if(ANOVA()$repr == repr || ZERO_INFLATED_NORMAL()$repr == repr || ZERO_INFLATED_NORMAL(sigma_per_group=T )$repr == repr){
    return(c(seq( -30, 0, length.out = 40), pos_log))
  }
  return(default)
}

model_cdf_plots <- function(model, trial_data,
                            scenario_name = "",
                            plot_path = "plots/model_distributions/",
                            limit=NULL) {

  if (scenario_name != "") {
    plot_path <- paste0(plot_path, scenario_name, "/")
  }

  dir.create(plot_path, recursive = TRUE, showWarnings = FALSE)

  source("R/evaluation/plot_functions/distribution_plot.R")

  CDFS <- trial_data$apply_to_each(function(trial) fit_model(model, trial)$get_CDFs(x), limit = limit)

  control_densities <- lapply(CDFS, function(CDF) CDF$control)
  treatment_densities <- lapply(CDFS, function(CDF) CDF$treatment)

  Y_control <- trial_data$all_data()[trial_data$all_data()$Group == "control", 1]
  Y_treatment <- trial_data$all_data()[trial_data$all_data()$Group == "treatment", 1]

  x <- get_x_range(model)

  p_control <- distributions_plot(control_densities, x, Y_control, paste0(model$name, " Distributions"))
  ggsave(paste0(plot_path, model$repr, "_control.pdf"), plot = p_control, width = 8, height = 5)

  p_treatment <- distributions_plot(treatment_densities, x, Y_treatment, paste0(model$name, " Distributions"))

  ggsave(paste0(plot_path, model$repr, "_treatment.pdf"), plot = p_treatment, width = 8, height = 5)


  return(list(control = p_control, treatment = p_treatment))
}