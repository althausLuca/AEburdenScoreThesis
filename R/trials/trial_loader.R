result_path <- "data/trials/"


#' Load a trial data from a scenario
#' @param scenario A scenario name or object with attribute name
#' @return A list of score data frames
#' @seealso \code{\link{simulate_scenario}} for simulating a scenario
#' @seealso \code{\link{simulate_scores_from_scenario}} for simulating a scenario
#'
load_trial_data <- function(file_name) {
  # check if it is not a string
  file_path <- paste0(result_path, file_name)
  if (!endsWith(file_name, ".csv")) {
    file_path <- paste0(result_path, file_name, ".csv")
  }
  if (!file.exists(file_path)) {
    stop(paste0("File not found for ", file_name))
  }

  trial_data <- read.table(file_path, header = FALSE, sep = "," )
  n_trials <- floor(nrow(trial_data)) / 2

  # Initialize a list to store the trial data frames
  trials <- vector("list", n_trials)

  # Loop over the trials
  for (i in seq_len(n_trials)) {
    # Get the control and treatment scores for the current trial
    group1 <- trial_data[2 * i - 1, 1]
    group2 <- trial_data[2 * i, 1]
    group1_scores <- unlist(trial_data[2 * i - 1,-1])
    groups2_scores <- unlist(trial_data[2 * i,  - 1])

    # Create a data frame with the scores and their corresponding group
    trial_df <- data.frame(
      Score = c(group1_scores, groups2_scores),
      Group = rep(c(group1, group2), each = length(group1_scores))
    )

    # Store the trial data frame in the list
    trials[[i]] <- trial_df
  }

  # Function to concatenate all trial data frames
  all_data <- function() {
    do.call(rbind, trials)
  }

  #function to apply a function to each trial
  apply_to_each <- function(func, as.df = FALSE, limit = NULL, ...) {
    # Determine the number of trials to process
    n <- if (is.null(limit)) length(trials) else min(limit, length(trials))

    # Initialize an empty list to store results
    results <- vector("list", n)

    # Loop over the trials and apply the function
    for (i in seq_len(n)) {
      results[[i]] <- func(trials[[i]], ...)
    }

    # Combine results into a data frame if as.df is TRUE
    if (as.df) {
      results <- do.call(rbind, results)
    }

    return(results)
  }


  # Return a list with n_trials, the trial data frames, and the all_data function
  return(list(n_trials = n_trials, trials = trials, all_data = all_data , apply_to_each = apply_to_each))
}



#' Load  trial data from shorter scenario
load_shorter_trials <- function(){
  file <- "Scenario_2_k_1.5_s_0.5.csv"
  trial_data <- load_trial_data(file)
  return(trial_data)
}

#' Load trial data from longer scenario
load_longer_trials <- function(){
  file <- "Scenario_3_k_1.5_l_3.5.csv"
  trial_data <- load_trial_data(file)
  return(trial_data)
}

#' Load trial data from longer scenario
load_equal_trials <- function(){
  file <- "Scenario_1_k_1.5.csv"
  trial_data <- load_trial_data(file)
  return(trial_data)
}





