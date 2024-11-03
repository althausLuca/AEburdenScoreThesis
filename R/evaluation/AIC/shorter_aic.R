library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)

# source("R/run_models/default_models.R")
source("R/helpers.R")
source("R/evaluation/config.R" , local = (eval_config <- new.env()))
source("R/evaluation/AIC/aic_model_map.R")

eval_config$PLOT_PATH

model_file_path <- eval_config$DEFAULT_GAP_TIME_VAR_FILE
file_name <- "AIC_shorter.pdf"
# model_computer <- load_model_computer(model_file_path)
# names(model_computer$models)

get_AIC_values <- function(file_path){
  model_computer <- load_model_computer(file_path)
  AIC_values <- get_value(model_computer, "AIC")
  AIC_values[] <- lapply(AIC_values, unlist)
  AIC_values <- subset(AIC_values, select = names(aic_model_map))
  return(AIC_values)
}

generate_aic_mean_table <- function(AIC_values , file_path = "experiments/AIC/mean_values.txt" ){
  dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)

  cat( "Model & Mean AIC",file = file_path)
  for(model_repr in names(aic_model_map)){
    model_name <- aic_model_map[[model_repr]]
    cat(model_name, " & ", round(mean(AIC_values[[model_repr]]), 2),  "\\\\ \n" , file = file_path, append = TRUE)
  }
  print("Mean AIC values written to file:")
  print(file_path)
}

aic_order <- function(AIC_values , file="" ){
  aic_order <- t(apply(AIC_values, 1,function(row) colnames(AIC_values)[order(row)]))

  model_reps <- colnames(AIC_values)
  n_ranks <- min(length(model_reps),10)

  aic_order[,1]
  cat("Rank &" , paste(1:n_ranks,collapse = " & "), "\n" , file = file)
  for(model_rep in model_reps){
      cat(aic_model_map[[model_rep]]  , file = file, append = TRUE)
      for(i in 1:n_ranks){
      count <- sum(aic_order[,i] == model_rep)
      cat(" & " , count, file = file, append = TRUE)
    }
    cat(" \\\\ \n" , file = file, append = TRUE)
  }
  return(aic_order)
}

aic_box_plots <- function(AIC_values){
  # transform df to long format
  AIC_df_long <- AIC_values %>%
    gather(key = "model", value = "AIC")

  model_names <- names(aic_model_map)
  model_names <- rev(model_names)
  model_labels <- sapply(model_names , function(repr) TeX(aic_model_map[[repr]]))

  AIC_df_long$model <- factor(AIC_df_long$model, levels = model_names)

  g <- ggplot(AIC_df_long, aes(x=model, y=AIC)) +
    geom_boxplot() + coord_flip() +
    stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                 width = .75, linetype = "dashed") +
    scale_x_discrete(labels = model_labels , breaks = model_names)

  #increase axis labels
  g <- g +  theme(axis.text.y = element_text(size = 14),
                  axis.text.x = element_text(size = 14),
                  axis.title.y = element_text(size = 0),
                  axis.title.x = element_text(size = 15),
  )
  g
  #save g to pdf
  ggsave(paste0(eval_config$PLOT_PATH
    , file_name), plot = g, device = "pdf", width = 20, height = 10, units = "cm")
  return(g)
}


AIC_values <- get_AIC_values(model_file_path)
generate_aic_mean_table(AIC_values)
aic_order <- aic_order(AIC_values)
aic_box_plots(AIC_values)
#
# model_computer <- load_model_computer(model_file_path)
# model <- model_computer$models$zero_inflated_normal_per_group
# add_model(model_computer, model , recompute = TRUE)