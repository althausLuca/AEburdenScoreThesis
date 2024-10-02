library(parallel)


init_trial_data <- function(trials_list) {

  n_trials <- length(trials_list)

  trial_data <- new.env()
  trial_data$trials <- trials_list
  trial_data$n_trials <- n_trials


  all_data <- function() {
    do.call(rbind, trial_data$trials)
  }

  #function to apply a function to each trial
  # use_parralel does not work on windows
  apply_to_each <- function(func, as.df = FALSE, limit = NULL, use_parallel = FALSE, ...) {
    # Determine the number of trials to process
    n <- if (is.null(limit)) length(trial_data$trials) else min(limit, length(trial_data$trials))

    cat("\n")

    func_silent <- function(i, trial, ...) {
      success <- FALSE
      on.exit({ if(!success) sink()})
    { sink("/dev/null");  ; result <- func(trial, ...); sink(); }
      cat("\r Processing trial ", i, " of ", n, " trials")
      success <- TRUE
      return(result)
    }

    if (use_parallel) {
      num_cores <- detectCores() - as.numeric(Sys.getenv("N_FREE_THREADS", 1))
      # Try parallel processing
      results <- try(mclapply(seq_len(n), function(i) func_silent(i, trial_data$trials[[i]], ...), mc.cores = num_cores), silent = TRUE)
      if (inherits(results, "try-error")) {
        print("Parallel processing failed, using sequential processing")
        use_parallel <- FALSE
      }
    }
    if (!use_parallel) {
      # Initialize an empty list to store results
      results <- vector("list", n)

      # Loop over the trials and apply the function
      for (i in seq_len(n)) {
        results[[i]] <- func_silent(i, trial_data$trials[[i]], ...)
      }
    }


    # Combine results into a data frame if as.df is TRUE
    if (as.df) {
      results <- data.frame(do.call(rbind, results))
    }

    return(results)
  }


  # Return a list with n_trials, the trial data frames, and the all_data function

  trial_data$apply_to_each <- apply_to_each
  trial_data$all_data <- all_data


  #result <- list(n_trials = n_trials, trials = trials_list, all_data = all_data, apply_to_each = apply_to_each)


  class(trial_data) <- "trial_data"
  return(trial_data)

  hist_and_box_plots <- function() {
    source("R/trials/analysis/plots.R")
    complete_df <- all_data()

    # Create a histogram of the scores
    hist(unlist(trial_data$trials$score), main = "Histogram of Scores", xlab = "Score", ...)
    # Create a boxplot of the scores
    boxplot(unlist(trial_data$trials$score), main = "Boxplot of Scores", ylab = "Score", ...)
  }

}

#override the lsit function to return a trial_data object
list.trial_data <- function(trial_data, ...) {
  print("list.trial_data conversion")
  return(trial_data$trials)
}

summary.trial_data <- function(trial_data, ...) {
  print(paste0("Number of trials: ", trial_data$n_trials))
  all_data <- trial_data$all_data()
  control_scores <- all_data$Score[all_data$Group == "control"]
  treatment_scores <- all_data$Score[all_data$Group == "treatment"]

  df <-data.frame(mean = c(mean(control_scores), mean(treatment_scores)),
             sd = c(sd(control_scores), sd(treatment_scores)),
             median = c(median(control_scores), median(treatment_scores)),
             zero = c(mean(control_scores == 0), mean(treatment_scores == 0)))

  row.names(df) <- c("control", "treatment")

  return(df)
}

save.trial_data <- function(trial_data, file_path) {
  # Open CSV file in append mode
  file.remove(file_path, showWarnings = FALSE)

  if (!dir.exists(dirname(file_path))) {
    dir.create(dirname(file_path), recursive = TRUE)
  }

  file_conn <- file(file_path, "a")

  for (index in seq_along(trial_data$trials)) {
    # Simulate control and treatment scores
    df <- trial_data$trials[[index]]

    control_scores <- df$Score[df$Group == "control"]
    treatment_scores <- df$Score[df$Group == "treatment"]

    # Write results to CSV file as one row
    write.table(as.data.frame(rbind(control_scores, treatment_scores), row.names = c("control", "treatment")),
                file = file_conn, sep = ",", col.names = FALSE, row.names = TRUE, append = TRUE)

  }
  close(file_conn)
}