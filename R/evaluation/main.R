# Execute this script to generate all plots for the simulation study section.

source("R/evaluation/config.R", local = (config <- new.env()))

source("R/evaluation/scenario_factor_variation/scenario_2.R")
source("R/evaluation/scenario_factor_variation/scenario_3.R")

## standard models
plot_duration_variation()
plot_gap_time_variation()

## QR models
plot_duration_variation(plot_settings = QR_MODEL_PLOT_SETTINGS,
                        plot_name = paste0(tools::file_path_sans_ext(config$DURATION_VARIATION_PLOT_PATH), "_qr.pdf"),
                        omit_arrow = TRUE
)

plot_gap_time_variation(plot_settings = QR_MODEL_PLOT_SETTINGS,
                        plot_name = paste0(tools::file_path_sans_ext(config$GAP_TIME_VARIATION_PLOT_PATH), "_qr.pdf"),
                        omit_arrow = TRUE
)

## sample size variation
source("R/evaluation/prop_of_p_values/trial_size_variation.R")


files_for_cdfs <- c(config$DEFAULT_GAP_TIME_VAR_FILE,
                    config$DEFAULT_DURATION_VAR_FILE)


#generate CDF plots for each model
source("R/evaluation/distribution_fit/plots_from_model_computer.R")
for (file in files_for_cdfs) {
  model_computer <- load_model_computer(file)

  plot_path <- paste0(config$PLOT_PATH, "model_distributions/", tools::file_path_sans_ext(basename(file)), "/")
  dir.create(plot_path, recursive = TRUE, showWarnings = FALSE)

  generate_CDF_plots(model_computer,
                     plot_path = paste0(config$PLOT_PATH,
                                        "model_distributions/",
                                        tools::file_path_sans_ext(basename(file)), "/"))
}


source("R/evaluation/scenario_factor_variation/scenario_4.R")
plot_severity_variation()


