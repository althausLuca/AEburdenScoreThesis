source("R/models_and_tests/model_computer.R")

scenario <- "shorter"

folder <- "results/sample_size_variation"
files <- list.files(folder, full.names = TRUE)
files <- files[grep(scenario, files)]

for(file in files) {
  print(file)
  model_computer <- load_model_computer(file)
  plot_name <- paste0("trial_size_variation_server/",scenario,"/", tools::file_path_sans_ext(basename(file)),".pdf")

  print(p_value_plot(model_computer,save=plot_name ,models_to_exclude = c("log_anova_c_10000", "tweedie_var_power_1.5_link_power_0" ,"zero_inflate_wilcoxon")))
}

names(model_computer$model_metrics)
names(model_computer$models)


p_observing_0 <- 0.3056
p_more_than_half_0 <- pbinom(9 , 20, p_observing_0 , lower.tail = TRUE )
p_more_than_half_0

