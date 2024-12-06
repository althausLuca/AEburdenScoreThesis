#log problem when running models to logs folder

init_logger <- function(trial_name="") {
  date <- Sys.Date()
  time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_folder <- paste0("R/run_models/logs/")
  log_file <- paste0(log_folder , trial_name, "_", date, "_", time, ".log")

  write <- function (message="Test") {
    #check if file exitsts
    if (!file.exists(log_file)) {
      dir.create(log_folder, recursive = TRUE, showWarnings = FALSE)
    }
    timed_message <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ", message)
    cat("\n" ,timed_message, file = log_file, append = TRUE)
  }

  return(list(write=write))
}