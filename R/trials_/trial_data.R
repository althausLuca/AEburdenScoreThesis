# library(parallel)
#
# init_trial_data <- function(trials_list) {
#   n_trials <- length(trials_list)
#
#   trial_data <- new.env()
#   trial_data$trials <- trials_list
#   trial_data$n_trials <- n_trials
#
#   trial_data$all_data <- function() {
#     do.call(rbind, trial_data$trials)
#   }
#
#
#   class(trial_data) <- "trial_data"
#   return(trial_data)
# }
#
#
# # apply a function to each trial
# #(use_parralel does not work on windows
# apply_to_trials <- function(trial_data, func, as.df = FALSE, limit = NULL, use_parallel = FALSE, ...) {
#   # Determine the number of trials to process
#   n <- if (is.null(limit)) length(trial_data$trials) else min(limit, length(trial_data$trials))
#   cat("\n")
#   func_silent <- function(i, trial, ...) {
#     success <- FALSE
#     on.exit({ if (!success) sink() })
#   {
#     # result <- func(trial, ...)
#     sink("/dev/null"); ; result <- func(trial, ...); sink();
#   }
#     cat("\r Processing trial ", i, " of ", n, " trials")
#     success <- TRUE
#     return(result)
#   }
#
#   if (FALSE) {
#     num_cores <- detectCores() - as.numeric(Sys.getenv("N_FREE_THREADS", 1))
#     # Try parallel processing
#     results <- try(mclapply(seq_len(n), function(i) func_silent(i, trial_data$trials[[i]], ...), mc.cores = num_cores), silent = TRUE)
#     if (inherits(results, "try-error")) {
#       print("Parallel processing failed, using sequential processing")
#       use_parallel <- FALSE
#     }
#   }
#   if (!use_parallel) {
#     # Initialize an empty list to store results
#     results <- vector("list", n)
#
#     # Loop over the trials and apply the function
#     for (i in seq_len(n)) {
#       results[[i]] <- func_silent(i, trial_data$trials[[i]], ...)
#     }
#   }
#
#   # Combine results into a data frame if as.df is TRUE
#   if (as.df) {
#     results <- data.frame(do.call(rbind, results))
#   }
#   return(results)
# }
#
#
# #override the lsit function to return a trial_data object
# list.trial_data <- function(trial_data, ...) {
#   print("list.trial_data conversion")
#   return(trial_data$trials)
# }
#
# summary.trial_data <- function(trial_data, ...) {
#   result <- list()
#   result$n_trials <- trial_data$n_trials
#   all_data <- trial_data$all_data()
#   control_scores <- all_data$Score[all_data$Group == "control"]
#   treatment_scores <- all_data$Score[all_data$Group == "treatment"]
#
#   df <- data.frame(mean = c(mean(control_scores), mean(treatment_scores)),
#                    sd = c(sd(control_scores), sd(treatment_scores)),
#                    median = c(median(control_scores), median(treatment_scores)),
#                    zero = c(mean(control_scores == 0), mean(treatment_scores == 0)))
#
#   row.names(df) <- c("control", "treatment")
#
#   result$summary <- df
#   return(result)
# }
#
#
# save.trial_data <- function(trial_data, file_path) {
#   # Open CSV file in append mode
#   file.remove(file_path, showWarnings = FALSE)
#
#   if (!dir.exists(dirname(file_path))) {
#     dir.create(dirname(file_path), recursive = TRUE)
#   }
#
#   file_conn <- file(file_path, "a")
#
#   for (index in seq_along(trial_data$trials)) {
#     # Simulate control and treatment scores
#     df <- trial_data$trials[[index]]
#
#     control_scores <- df$Score[df$Group == "control"]
#     treatment_scores <- df$Score[df$Group == "treatment"]
#
#     # Write results to CSV file as one row
#     write.table(as.data.frame(rbind(control_scores, treatment_scores), row.names = c("control", "treatment")),
#                 file = file_conn, sep = ",", col.names = FALSE, row.names = TRUE, append = TRUE)
#
#   }
#   close(file_conn)
# }