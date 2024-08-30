source("R/models_and_tests/model_computer.R")

scenario <- "longer"


folder <- "results/sample_size_variation"
files <- list.files(folder, full.names = TRUE)

equal_files <- files[grep(scenario, files)]

for(file in equal_files) {
  print(file)
  model_computer <- load_model_computer(file)

  plot_name <- paste0("trial_size_variation_server/",scenario,"/", tools::file_path_sans_ext(basename(file)),".pdf")

  print(p_value_plot(model_computer,save=plot_name ,models_to_exclude = c("log_anova_c_10000", "tweedie_var_power_1.5_link_power_0")))
}


names(model_computer$models)

