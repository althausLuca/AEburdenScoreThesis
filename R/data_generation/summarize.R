#' Summarize all trials from the TRIAL_DATA_PATH currently set in the config_and_init.R file.
#' Results are stored in trial_summary.txt and show  wheterver the data is computed and
#' if so, the summary of the trials containing mean, sd, median and zero values per treatment group

source("R/data_generation/config_and_init.R")
path <- TRIAL_DATA_PATH

result_path <- "trial_summary.txt"

trial_files <- list.files(path , full.names = TRUE , recursive = TRUE, pattern = ".RData")


cat("TRIAL SUMMARY \n", file = result_path)

for(file in trial_files){
  print(file)
  trial_data <- load.trial_data(file)
  s <- summary.trial_data(trial_data)
  cat(paste0(file, "\n"), file = result_path , append = TRUE)
  suppressWarnings(
  write.table(s, file = result_path, append = TRUE,row.names = FALSE)
  )
  cat(paste0("Trial:", trial_data$n_trials,"\n"), file = result_path, append = TRUE)
  cat("\n", file = result_path, append = TRUE)
}

