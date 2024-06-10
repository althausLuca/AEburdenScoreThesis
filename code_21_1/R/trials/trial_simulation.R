result_path <- "/../data/trials"

source("R/AdverseEvents/AdverseEvents.R")
source("R/AdverseEvents/event_simulation.R")

#' Simulate a scenarios
#'
#' This function simulates a scenario and saves the results to a json file.
simulate_trials_from_scenario <- function(scenario, n_sim = 1000, death = FALSE) {
  print(scenario$name)
  print("generating scores")

  trial_list <- list()
  for (index in 1:n_sim) {
    simulated_trial <- simulate_trial(scenario$treatment_AEs, scenario$control_AEs, 180,
                                      sample_size = scenario$observations, death = death)
    trial_list <- c(trial_list, list(simulated_trial))
    if (index %% 20 == 0) {
      print(index / n_sim)
    }
  }
  if (death) {
    saveRDS(trial_list, file = paste0(result_path, "/", scenario$name, "_death.rds"))
  }else {
    saveRDS(trial_list, file = paste0(result_path, "/", scenario$name, ".rds"))
  }
}


#' Simulate  a trial from a scenario
#' @param scenario A scenario object from SCENARIOS with attributes treatment_AEs and control_AEs (lists of AEs)
#' @param trial_duration duration of the trial
#' @return a dataframe with scores and group
#' @seealso \code{\link{simulate_scenario}} for simulating a scenario
#' @seealso \code{\link{simulate_scores_from_scenario}} for simulating a scenario
#'
simulate_trial_from_scenario <- function (scenario, trial_duration ){
  subjects.treatment <- create_subjects(scenario$treatment_AEs, susceptibility_parameter = list("gamma", 2), group="treatment")
  subjects.control <- create_subjects(scenario$control_AEs, susceptibility_parameter = list("gamma", 2), group="control")

  simulated_trial <- simulate_trial(subjects.treatment, subjects.control, trial_duration)

  control.scores <- sapply(simulated_trial$control, function(s) s$score)
  treatment.scores <- sapply(simulated_trial$treatment, function(s) s$score)

  score_df <- data.frame(score=c(control.scores, treatment.scores), group=c(rep("control", length(control.scores)), rep("treamtment", length(treatment.scores))))
}

#' Load a scores from a scenario
#' @param scenario A scenario object from SCENARIOS
#' @return A list of score data frames
#' @seealso \code{\link{simulate_scenario}} for simulating a scenario
#' @seealso \code{\link{simulate_scores_from_scenario}} for simulating a scenario
#'
load_trials <- function(scenario) {
print(paste0("loading ", scenario$name, "  scores"))
trial_list <- readRDS(file = paste0(result_path, "/", scenario$name, ".rds"))
return(trial_list)
}


#' Simulate a trial with multiple events
#' This function simulates multiple events based on the provided subjects parameters.
#' @param subjects.treatment
#' @param subjects.control
#' @param max_time Maximum time to simulate events (in days)
#' @return A list containint treatement : a list of simulated AEs for the treatment group
#' and control : a list of simulated AEs for the control group
#' @seealso \code{\link{AE}} for defining an event
#' @seealso \code{\link{simulate_event}} for simulating a single event
#'
#' simulated_AEs <- simulate_events(AEs.treatment, AEs.control , 180)
simulate_trial <- function(subjects.treatment, subjects.control, max_time, death = FALSE) {

results_treatment <- lapply(subjects.treatment, function(s) simulate_subject(s, max_time))
results_control <-  lapply(subjects.control, function(s) simulate_subject(s, max_time))

results <- list(
treatment = results_treatment,
control = results_control
)
return(results)
}

#' Simulate events for a subject considering susceptibility
#' This function simulates multiple events based on the provided event parameters.
#' @param subject A subject object
#' @param max_time Maximum time to simulate events (in days)
#' @param death Boolean indicating whether to simulate death
simulate_subject <- function(subject, max_time, death = FALSE) {
AEs <- subject$AEs
simulated_events <- simulate_events(AEs, max_time, death = death , susceptibility = subject$susceptibility)

subject$AEs <- simulated_events$AEs
subject$score<- simulated_events$score
return(subject)
}