source("R/models_and_tests/models_and_tests.R")
source("R/trials/trial_loader.R")
source("R/evaluation/distribution_fit/x.R")

#ZERO_INFLATED_LOGNORMAL(),ANOVA() , LOG_ANOVA(c = 0.001), LOG_ANOVA(c = 1) ,
models <- list(  TWEEDIE_REGRESSION(xi = 1.2),  TWEEDIE_REGRESSION(xi = 1.5),  TWEEDIE_REGRESSION(xi = 1.8)  )

trial_data <- load_longer_trials()
scenario_name <- "longer_events"

for(model in models){

CDFS <- trial_data$apply_to_each(function(trial) fit_model(model, trial)$get_CDFs(x))

control_densities <- lapply(CDFS, function(CDF) CDF$control)
treatment_densities <- lapply(CDFS, function(CDF) CDF$treatment)

Y_control <- trial_data$all_data()[trial_data$all_data()$Group == "control", 1]
Y_treatment <- trial_data$all_data()[trial_data$all_data()$Group == "treatment", 1]


source("R/evaluation/analysis_and_comparison/distribution_plot.R")
plot_path <- paste0("plots/model_distributions/",scenario_name,"/")

dir.create(plot_path, recursive = TRUE, showWarnings = FALSE)

p <- distributions_plot(control_densities, x, Y_control, paste0(model$name , " Distributions"))
ggsave(paste0(plot_path, model$repr, "_control.pdf"), plot = p, width = 8, height = 5)

p <- distributions_plot(treatment_densities, x, Y_treatment, paste0(model$name , " Distributions"))

ggsave(paste0(plot_path, model$repr, "_treatment.pdf"), plot = p, width = 8, height = 5)

}

trial_data <- load_shorter_trials()
scenario_name <- "shorter_gap_times"

for(model in models){

  CDFS <- trial_data$apply_to_each(function(trial) fit_model(model, trial)$get_CDFs(x))

  control_densities <- lapply(CDFS, function(CDF) CDF$control)
  treatment_densities <- lapply(CDFS, function(CDF) CDF$treatment)

  Y_control <- trial_data$all_data()[trial_data$all_data()$Group == "control", 1]
  Y_treatment <- trial_data$all_data()[trial_data$all_data()$Group == "treatment", 1]


  source("R/evaluation/analysis_and_comparison/distribution_plot.R")
  plot_path <- paste0("plots/model_distributions/",scenario_name,"/")

  dir.create(plot_path, recursive = TRUE, showWarnings = FALSE)

  p <- distributions_plot(control_densities, x, Y_control, paste0(model$name , " Distributions"))
  ggsave(paste0(plot_path, model$repr, "_control.pdf"), plot = p, width = 8, height = 5)

  p <- distributions_plot(treatment_densities, x, Y_treatment, paste0(model$name , " Distributions"))

  ggsave(paste0(plot_path, model$repr, "_treatment.pdf"), plot = p, width = 8, height = 5)

}
