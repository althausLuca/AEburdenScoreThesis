source("R/models_and_tests/model_computer.R")
source("R/evaluation/config.R", local = (eval_config <- new.env()))
source("R/evaluation/prop_of_p_values/p_value_plot.R")

#top level folder


folder <- eval_config$TRIAL_SIZE_VARIATION_PATH
files <- list.files(folder, full.names = TRUE , recursive = TRUE)
files <- c(files, eval_config$EQUAL_SETTIGS_FILE , eval_config$DEFAULT_DURATION_VAR_FILE , eval_config$DEFAULT_GAP_TIME_VAR_FILE)

for(file in files) {
  result_name <- paste0(eval_config$PLOT_PATH, basename(folder) ,
                        "/", tools::file_path_sans_ext(basename(file)),".pdf")

  LEGEND_ON_TOP <<- grepl("equal" , file) || grepl("s1" , file)

  model_computer <- load_model_computer(file)
  p_values <- get_value(model_computer, "p_value")
  p <- p_value_plot(p_values , save =result_name,
                    models_to_include = eval_config$model_reprs)
}
print(p)
