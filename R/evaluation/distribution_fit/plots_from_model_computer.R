source("R/evaluation/distribution_fit/score_range.R")
source("R/models_and_tests/models_and_tests.R") # to init get_CDFs functions
source("R/evaluation/distribution_fit/distribution_plot.R")

#uses
#model_rep -> model_def_function
#model_rep -> list of estimates per trial
generate_CDF_plots <- function(model_computer, model_reps_to_plot = NULL,
                               plot_path = "plots/method_evaluation/model_distributions/") {

  dir.create(plot_path , recursive = TRUE, showWarnings = FALSE)

  trial_data <- model_computer$trial_data
  Y_control <- trial_data$all_data()[trial_data$all_data()$Group == "control", 1]
  Y_treatment <- trial_data$all_data()[trial_data$all_data()$Group == "treatment", 1]

  models <- model_computer$models
  models_to_plot <- list() # create list of models to plot
  for (i in seq_along(models)) {
    model_repr <- names(models)[[i]]
    model <- models[[i]]
    stopifnot(model_repr == model$repr)

    if (!is.null(model_reps_to_plot) && !(model_repr %in% model_reps_to_plot)) {
      next
    }
    if (!any(grepl(class(model)[[1]], methods("get_CDFs")))) { #check if get_CDFs method exists for model
      warning(paste0("No get_CDFs function found for model ", model$repr))
      next
    }
    models_to_plot[[model_repr]] <- model
  }

  for(model in models_to_plot){
    print("generating plots for:")
    print(model$repr)

    model_repr <- model$repr
    trial_n <- nrow(model_computer$model_estimates[[model_repr]])

    x <- get_x_range(model)
    CDFS <- lapply(1:trial_n, function(k) get_CDFs(model, x, estimates = model_computer$model_estimates[[model_repr]][k,]))
    control_distributions <- lapply(CDFS, function(CDF) CDF$control)
    treatment_distributions <- lapply(CDFS, function(CDF) CDF$treatment)

    p_control <- distributions_plot(control_distributions, x, Y_control, paste0(model$name, " Distributions"))
    ggsave(paste0(plot_path, model_repr, "_control.pdf"), plot = p_control, width = 8, height = 5)

    p_treatment <- distributions_plot(treatment_distributions, x, Y_treatment, paste0(model$name, " Distributions"))
    ggsave(paste0(plot_path, model_repr, "_treatment.pdf"), plot = p_treatment, width = 8, height = 5)

  }
}



