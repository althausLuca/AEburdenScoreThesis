source("R/trials/trial_data.R")

#' Get the file path for a given file name
#' @param file_name A file name or path
#' @param result_path The path to the result files (defualts to "data/trials/")
#' @return The file path for the given file name or stop if the file does not exist
get_file_path <- function(file_name,result_path= "data/trials/") {
  if (!endsWith(file_name, ".csv")) {
    file_name <- paste0(file_name, ".csv")
  }
  file_path <- paste0(result_path, file_name)

  if (!file.exists(file_path)) {
    # check if in file_path + longer_event_durations or shorter_gap_times
    file_path <- paste0(result_path, "longer_event_durations/", file_name)
    if (!file.exists(file_path)) {
      file_path <- paste0(result_path, "shorter_gap_times/", file_name)
      if(!file.exists(file_path))  stop(paste0("File not found for ", file_name))
    }
  }
  return(file_path)
}

#' Get the control and treatment scores for the current trial
#' @param i The trial number
#' @param trial_data The trial data frame
#' @return A data frame with the scores and group names
#' @seealso \code{\link{get_trial_data}} for loading trial data
get_lines <- function(i, trial_data) {
  group1 <- trial_data[2 * i - 1, 1]
  group2 <- trial_data[2 * i, 1]
  group1_scores <- as.numeric(trial_data[2 * i - 1,-1])
  groups2_scores <- as.numeric(trial_data[2 * i,  - 1])

  trial_df <- data.frame(
    Score = c(group1_scores, groups2_scores),
    Group = rep(c(group1, group2), each = length(group1_scores))
  )
}


#' Load a trial data from a scenario
#' @param scenario A scenario name or object with attribute name
#' @return A list of score data frames
#' @seealso \code{\link{simulate_scenario}} for simulating a scenario
#' @seealso \code{\link{simulate_scores_from_scenario}} for simulating a scenario
get_trial_data <- function(file_name , result_path = "data/trials/") {

  # check if it is not a string
  file_path <- get_file_path(file_name ,result_path =  result_path)

  trial_data <- read.table(file_path, header = FALSE, sep = "," )
  n_trials <- floor(nrow(trial_data)) / 2

  # Initialize a list to store the trial data frames
  trials <- vector("list", n_trials)

  # Loop over the trials
  for (i in seq_len(n_trials)) {
    # Store the trial data frame in the list
    trials[[i]] <- get_lines(i,trial_data)
  }
  return(init_trial_data(trials))
}



#' Load a subsample form trial data
#' @param trial A trial data frame
#' @param group_size The number of subjects per group
#' @return A subsample of the trial data
trial_sub_sampler <- function(trial, group_size=30){
  ref_group <- "control"

  ref_incdices <- which(ref_group == trial$Group)[1:group_size]
  treatment_indices <- which(ref_group != trial$Group)[1:group_size]

  indices_to_keep <- c(ref_incdices,treatment_indices)

  trial <- trial[indices_to_keep,]

  return(trial)

}


#' Load  trial data from the scenariio with shorter gap times
load_shorter_trials <- function(){
  file <- "s2_k_1.5_s_0.5.csv"
  trial_data <- get_trial_data(file)
  trial_data[["name"]] <- "shorter_gap_times"
  return(trial_data)
}

#' Load trial data from the scenario with longer event times
load_longer_trials <- function(){
  file <- "s3_k_1.5_l_3.5.csv"
  trial_data <- get_trial_data(file)
  trial_data[["name"]] <- "longer_events"
  return(trial_data)
}

#' Load trial data from the scenario with equal parameters
load_equal_trials <- function(){
  file <- "s3_k_1.5_l_1.csv"
  trial_data <- get_trial_data(file)
  trial_data[["name"]] <- "equal"
  return(trial_data)
}

#default scenarios
default_scenario_loaders <- list(
  shorter_gap_times = load_shorter_trials,
  longer_events = load_longer_trials,
  equal = load_equal_trials
)




