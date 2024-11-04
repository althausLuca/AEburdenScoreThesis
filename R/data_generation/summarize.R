source("R/data_generation/config_and_init.R")
source("R/data_generation/trial_data.R")
path <- TRIAL_DATA_PATH

result_path <- "trial_summary.txt"

trial_files <- list.files(path , full.names = TRUE , recursive = TRUE, pattern = ".RData")


cat("TRIAL SUMMARY \n", file = result_path)

for(file in trial_files){
  print(file)
  trial_data <- load.trial_data(file)
  s <- summary.trial_data(trial_data)
  cat(paste0(file, "\n"), file = result_path , append = TRUE)
  cat(paste0(s), file = result_path, append = TRUE)
  cat(paste0("\n", trial_data$n_trials,"\n"), file = result_path, append = TRUE)
  cat("\n", file = result_path, append = TRUE)
}

