source("R/evaluation/prop_of_p_values/config.R", local = (config <- new.env()))
source("R/helpers.R")


get_p_value_df <- function(folder, factor_prefix = "_s_", models_reprs = config$model_reprs) {
  model_files <- list.files(folder, full.names = TRUE)
  print(model_files)
  df <- NULL
  for (model_file in model_files) { # takes a while
    scenario_factor <- get_prefixed_number(model_file, factor_prefix)
    print(scenario_factor)

    model_computer <- load_model_computer(model_file)

    p_values <- get_value(model_computer, "p_value")
    p_values <- subset(p_values, select = models_reprs)
    sig_p_values <- colMeans(p_values < 0.05, na.rm = TRUE)
    print(sig_p_values)
    for (model in names(sig_p_values)) {
      if (model %in% models_reprs) {
        df <- rbind(df, c(scenario_factor = scenario_factor,
                          model = model,
                          value = unname(sig_p_values[model])))
      }
    }
  }
  df <- data.frame(df)
  df$value <- as.numeric(df$value)
  return(df)
}