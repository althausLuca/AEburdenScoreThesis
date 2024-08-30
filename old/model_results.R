source("R/models/analysis_and_comparison/p_value_plot.R")
source("R/helpers.R")


init_model_results <- function(model_results) {
  if(!is.data.frame(model_results)){
    model_results <- nested_list_to_data_frame(model_results)
  }
  return(init_model_results_(model_results))
}

init_model_results_ <- function(result_df) {
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

    models_with_value <- model_names[models_contain_value]

    if (length(models_with_value) == 0) {
      print(paste0("No model with the given pattern: ", pattern))
      return(NULL)
    }

    columns_to_select <- sapply(models_with_value, function(model_name) {
      paste0(model_name, ".", pattern)
    })
    result <- result_df[, columns_to_select, drop = FALSE]
    names(result) <- models_with_value

    return(result)
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


  #' Add model results
  #' @param model_results: data.frame with model results
  #' @return: list with model results
  #' @examples
  #' model_results <- data.frame(anova.model = rep("anova",10), "anova.p_value" = rep(0.05,10))
  #' model_results <- add_model_results(model_results)
  add_model_results <- function(model_results) {
    if(!is.data.frame(model_results)){
      model_results <- nested_list_to_data_frame(model_results)
    }
    result_df <- rbind(result_df, model_results)
    return(init_model_results_(result_df))
  }

  return(list( results = result_df ,
               model_names = model_names,
               model_attributes = model_attributes,
               get_values = get_values ,
               p_value_plot = p_value_plot ,
               add_model_results = add_model_results))
}

load_model_results_from_csv <- function(filename="Scenario_1_k_1.5.csv", path="data/model_results/", ...) {
  file <- paste0(path, filename)
  result <- read.csv(file , comment.char = '#' ,...)
  return(init_model_results_2(result))
}

