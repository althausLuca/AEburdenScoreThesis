source("methods_settings.R")
source("R/methods/run_methods.R")

file_dir <- "data/model_results/"
file_name  <- "scenario_1.csv"

result_df <- read.table(paste0(file_dir, file_name), header = TRUE, sep = ",")
p_values <- get_values(result_df, "p_value")


# count the number of p-values less than 0.05 per model/column
significant_per_col_values <- sapply(p_values, function(x) sum(x < 0.05))
"n sim"

nrow(p_values)
significant_per_col_values


file_name  <- "scenario_1_c.csv"

result_df <- read.table(paste0(file_dir, file_name), header = TRUE, sep = ",")
p_values <- get_values(result_df, "p_value")

# count the number of p-values less than 0.05 per model/column
significant_per_col_values <- sapply(p_values, function(x) sum(x < 0.05))
"n sim"

nrow(p_values)
significant_per_col_values
