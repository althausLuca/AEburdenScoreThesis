source("R/data_generation/config_and_init.R")
source("R/trials/trial_loader.R")
path <- TRIAL_DATA_PATH

result_path <- "trial_summary.txt"

trial_files <- list.files(path , full.names = TRUE , recursive = TRUE, pattern = ".csv")

file <- trial_file[1]

cat("TRIAL SUMMARY \n", file = result_path)

for(file in trial_files){
  print(file)
  trial_data <- get_trial_data(file, result_path = "")
  s <- summary.trial_data(trial_data)
  cat(paste0(file, "\n"), file = result_path , append = TRUE)
  cat(paste0(s$summary), file = result_path, append = TRUE)
  cat("\n", file = result_path, append = TRUE)
  cat("\n", file = result_path, append = TRUE)
}

