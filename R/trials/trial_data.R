library(parallel)


init_trial_data <- function(trials_list){

  all_data <- function() {
    do.call(rbind, trials_list)
  }

  #function to apply a function to each trial
  # use_parralel does not work on windows
  apply_to_each <- function(func, as.df = FALSE, limit = NULL, use_parallel=FALSE , ...) {
    # Determine the number of trials to process
    n <- if (is.null(limit)) length(trials_list) else min(limit, length(trials_list))


    if(use_parallel){
      num_cores <- detectCores() - as.numeric(Sys.getenv("N_FREE_THREADS",1))
      # Try parallel processing
      results <- try(mclapply(seq_len(n), function(i) func(trials_list[[i]], ...), mc.cores = num_cores), silent = TRUE)
      if (inherits(results, "try-error")) {
        print("Parallel processing failed, using sequential processing")
        use_parallel <- FALSE
      }
    }
    if(!use_parallel){
      # Initialize an empty list to store results
      results <- vector("list", n)

      # Loop over the trials and apply the function
      for (i in seq_len(n)) {
        results[[i]] <- func(trials_list[[i]], ...)
      }
    }


    # Combine results into a data frame if as.df is TRUE
    if (as.df) {
      results <- do.call(rbind, results)
    }

    return(results)
  }

  n_trials <- length(trials_list)

  # Return a list with n_trials, the trial data frames, and the all_data function

  result <- list(n_trials = n_trials, trials = trials_list, all_data = all_data , apply_to_each = apply_to_each)
  # set class of result to "trial_data"
  class(result) <- "trial_data"
  return(result)


  hist_and_box_plots <- function(){
    source("R/trials/analysis/plots.R")
    complete_df <- all_data()


    # Create a histogram of the scores
    hist(unlist(trial_data$trials$score), main = "Histogram of Scores", xlab = "Score", ...)
    # Create a boxplot of the scores
    boxplot(unlist(trial_data$trials$score), main = "Boxplot of Scores", ylab = "Score", ...)
  }
}

#override the lsit function to return a trial_data object
list.trial_data <- function(trial_data,...){
  print("list.trial_data conversion")
  return(trial_data$trials)
}