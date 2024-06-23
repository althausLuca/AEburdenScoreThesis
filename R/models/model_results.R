source("R/models/analysis_and_comparison/p_value_plot.R")


init_model_results <- function(result_list) {
  if(is.data.frame(result_list)){
   return(init_model_results_2(result_list))
  }

  # Get the first entry from the result list
  first_entry <- result_list[[1]]
  model_names <- names(first_entry)

  # Function to get values based on a pattern
  get_values <- function(pattern = "p_value") {
    # Check which models contain the pattern in their names
    models_contain_value <- sapply(model_names, function(model_name) {
      pattern %in% names(first_entry[[model_name]])
    })

    n_models_with_value <- sum(models_contain_value)
    if (n_models_with_value == 0) {
      print(paste0("No model with the given pattern: ", pattern))
      return(NULL)
    }

    # Initialize a data frame to store the values
    df <- data.frame(matrix(nrow = length(result_list), ncol = n_models_with_value))
    colnames(df) <- model_names[models_contain_value]

    # Loop through the result list and extract values
    for (i in seq_along(result_list)) {
      entry <- result_list[[i]]
      values <- sapply(model_names[models_contain_value], function(model_name) {
        entry[[model_name]][[pattern]]
      })
      df[i, ] <- values
    }

    return(df)
  }


  p_value_plot <- function (save = NULL) {

    p_values <- get_values("p_value")
    p_value_plot <- p_value_plot_handler()
    for(name in model_names){
      p_value_plot$add(p_values[[name]], name)
    }
    if(!is.null(save)){
      p_value_plot$save(save)
    }
    result <- p_value_plot$plot()
    return(result)
  }

  return(list( results = result_list ,
               get_values = get_values ,
               model_names = model_names,
               p_value_plot = p_value_plot))
}

init_model_results_2 <- function(result_df) {
  # Get the first entry from the result list
  df_columns <- names(result_df)
  dot_splitted_columns <- strsplit(df_columns, "(?![0-9])\\.(?![0-9])", perl = TRUE) # split by dot if not surrounded by numbers
  model_names <-  unique(sapply(dot_splitted_columns , function(x) x[1]))
  #map model_name to -> list of atributes
  model_attributes <- setNames(lapply(model_names, function(t_) list()), model_names)
  names(model_attributes) <- model_names
  for (split in dot_splitted_columns) {
    model_attributes[[split[1]]] <- c(model_attributes[[split[1]]], paste0(split[2:length(split)], collapse = "."))
  }
  # Function to get values based on a pattern
  get_values <- function(pattern = "p_value") {
    # Check which models contain the pattern in their names
    models_contain_value <- sapply(model_names, function(model_name) {
      pattern %in% model_attributes[[model_name]]
    })

    n_models_with_value <- sum(models_contain_value)
    if (n_models_with_value == 0) {
      print(paste0("No model with the given pattern: ", pattern))
      return(NULL)
    }

    # Initialize a data frame to store the values
    df <- data.frame(matrix(nrow = nrow(result_df), ncol = n_models_with_value))
    colnames(df) <- model_names[models_contain_value]

    # Loop through the result list and extract values
    for (model_name in model_names[models_contain_value]) {
        df[, model_name] <- result_df[, paste0(model_name, ".", pattern)]
    }

    return(df)
  }


  p_value_plot <- function (save = NULL) {
    source("R/models/analysis_and_comparison/p_value_plot.R")

    p_values <- get_values("p_value")
    p_value_plot <- p_value_plot_handler()
    for(name in model_names){
      p_value_plot$add(p_values[[name]], name)
    }
    if(!is.null(save)){
      p_value_plot$save(save)
    }
    result <- p_value_plot$plot()
    return(result)
  }

  return(list( results = result_df ,
               model_names = model_names,
               model_attributes = model_attributes,
               get_values = get_values ,
               p_value_plot = p_value_plot))
}

load_model_results_from_csv <- function(filename="Scenario_1_k_1.5.csv", path="data/model_results/", ...) {
  file <- paste0(path, filename)
  result <- read.csv(file , comment.char = '#' ,...)
  return(init_model_results_2(result))
}

#
# file <- "data/workspaces/smaller_samples_smaller_20.RData"
# load(file)
# model_results <- init_model_results(result)
#
#
# model_results$get_values("p_value")

model_results <- load_model_results_from_csv("Scenario_1_k_1.5.csv")
model_results$get_values("p_value")


