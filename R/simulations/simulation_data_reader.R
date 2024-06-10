
source("R/methods/run_methods.R")

data_folder <- "data_1500_1000/model_results"
alpha <- 0.05

# get all the files in the foldeer from scenario 1
files.1 <- list.files(data_folder, pattern = "Scenario_1")

for(file.1 in files.1) {
  # get the scenario name
  k <- strsplit(file.1, "_")[[1]][4]
  k <- substr(k, 1, nchar(k) - 4)

  print(k)

  # load data
  data <- read.csv(paste0(data_folder, "/", file.1))
  p_values <- get_values(data, "p_value")
  print(colMeans(p_values < alpha))
}





files.2 <- list.files(data_folder, pattern = "Scenario_2")
files.2 <- files.2[grepl("_s_", files.2)]
files.2

for(file.2 in files.2) {
  # get the scenario name
  k <- strsplit(file.2, "_")[[1]][4]
  s <- strsplit(file.2, "_")[[1]][6]
  # remove .csv
  s <- substr(s, 1, nchar(s) - 4)

  print(k)
  print(s)

  # load data
  data <- read.csv(paste0(data_folder, "/", file.2))
  p_values <- get_values(data, "p_value")
  print(colMeans(p_values < alpha))
}
q_rank_p <- p_values$quantile_regression.rank
p_values$quantile_regression[is.na(q_rank_p)]

files.3 <- list.files(data_folder, pattern = "Scenario_3")
files.3 <- files.3[grepl("_l_", files.3)]
files.3 <- files.3[grepl("_k_", files.3)]
files.3

for(file.3 in files.3) {
  # get the scenario name
  k <- strsplit(file.3, "_")[[1]][4]
  l <- strsplit(file.3, "_")[[1]][6]
  # remove .csv
  l <- substr(l, 1, nchar(l) - 4)

  print(k)
  print(l)

  data <- read.csv(paste0(data_folder, "/", file.3))
  p_values <- get_values(data, "p_value")
  print(colMeans(p_values < alpha))
}

files.4 <- list.files(data_folder, pattern = "Scenario_4")
files.4 <- files.4[grepl("_k_", files.4)]
files.4

for (file.4 in files.4) {
  # get the scenario name
  k <- strsplit(file.4, "_")[[1]][4]
  # remove .csv
  k <- substr(k, 1, nchar(k) - 4)

  print(k)

  # load data
  data <- read.csv(paste0(data_folder, "/", file.4))
  p_values <- get_values(data, "p_value")
  print(colMeans(p_values < alpha))
}


