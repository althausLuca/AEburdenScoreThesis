source("R/evaluation/config.R", local = (eval_config <- new.env()))
source("R/evaluation/prop_of_p_values/p_value_cdf_plot.R")
#top level folder


folder <- eval_config$TRIAL_SIZE_VARIATION_PATH
files <- list.files(folder, full.names = TRUE , recursive = TRUE)
files <- c(files, eval_config$EQUAL_SETTIGS_FILE , eval_config$DEFAULT_DURATION_VAR_FILE , eval_config$DEFAULT_GAP_TIME_VAR_FILE)

for(file in files) {
  result_name <- paste0(eval_config$PLOT_PATH, basename(folder) ,
                        "/", tools::file_path_sans_ext(basename(file)),".pdf")

  dir.create(dirname(result_name), recursive = TRUE , showWarnings = FALSE)

  legend_top_left <- grepl("equal" , file) || grepl("s1" , file)

  model_computer <- load_model_computer(file)
  p_value_plot(model_computer, save = result_name , legend_top_left = legend_top_left)
}
