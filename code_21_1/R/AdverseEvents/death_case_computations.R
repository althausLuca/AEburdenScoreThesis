source("R/scenario_configuration.R")

#' Get death time of an event
#' @param simulated_AE A simulated Adverse Event containing the info data frame
#' @param mild The time until death for a mild AE
#' @param moderate The time until death for a moderate AE
#' @param severe The time until death for a severe AE
#' @return The time of death w.r.t to the AE
#' @seealso \code{\link{simulate_event}} for simulating an event
get_AE_time <- function(simulated_AE,
                        time.mild = TIME_UNTIL_DEATH[MILD],
                        time.moderate = TIME_UNTIL_DEATH[MODERATE],
                        time.severe = TIME_UNTIL_DEATH[SEVERE]) {
  max_time <- Inf
  event_info <- simulated_AE$info

  # print("mild")
  # print(time.mild)
  # print(event_info$Duration > time.mild)
  # print(event_info$Severity == MILD)
  indices <- which(event_info$Severity == MILD & event_info$Duration > time.mild)
  min_time_mild <- ifelse(length(indices) == 0, max_time, event_info$Start[min(indices)] + TIME_UNTIL_DEATH[MILD])

  indices <- which(event_info$Severity == MODERATE & event_info$Duration > time.moderate)
  min_time_moderate <- ifelse(length(indices) == 0, max_time, event_info$Start[min(indices)] + severe)

  indices <- which(event_info$Severity == SEVERE & event_info$Duration > time.severe)
  min_time_severe <- ifelse(length(indices) == 0, max_time, event_info$Start[min(indices)] + TIME_UNTIL_DEATH[SEVERE])

  return(min(min_time_mild, min_time_moderate, min_time_severe))
}

#' Get death time of event list
#' @param simulated_AE A simulated AE
#' @param mild The time until death for a mild AE
#' @param moderate The time until death for a moderate AE
#' @param severe The time until death for a severe AE
#' @return The time of death of the patient
#' @seealso \code{\link{simulate_event}} for simulating an event
get_death_time <- function(simulated_AEs,
                           time.mild = TIME_UNTIL_DEATH[MILD],
                           time.moderate = TIME_UNTIL_DEATH[MODERATE],
                           time.severe = TIME_UNTIL_DEATH[SEVERE]) {
  death_time <- c()
  for (i in 1:length(simulated_AEs)) {
    death_time <- c(death_time,
                    get_AE_time(simulated_AEs[[i]],
                                time.mild = time.mild, time.moderate = time.moderate, time.severe = time.severe)
    )
  }
  print("AAAAAAAAAAAA")
  min_death_time <- min(death_time)
  print("death time")
  print(min_death_time)
  return(min_death_time)
}

#' Set death time
#' Restet the Start End and Severity of all events after the death of the patient
#' @param simulated_AEs A list of simulated AEs or a list where AEs are an element of the list
#' @return A list of simulated AEs with the death time set
set_death <- function(simulated_AEs,
                      time.mild = TIME_UNTIL_DEATH[MILD],
                      time.moderate = TIME_UNTIL_DEATH[MODERATE],
                      time.severe = TIME_UNTIL_DEATH[SEVERE]) {

  AEs_are_element <- "AEs" %in% names(simulated_AEs)
  print(AEs_are_element)
  if (AEs_are_element) {
    AEs <- simulated_AEs$AEs
  }
  else {
    AEs <- simulated_AEs
  }

  min_death_time <- get_death_time(AEs,
                                   time.mild = time.mild, time.moderate = time.moderate, time.severe = time.severe)

  if (min_death_time == Inf) {
    if (AEs_are_element) {
      simulated_AEs$score_without_death <- simulated_AEs$score
      return(simulated_AEs)
    }
    else {
      return(AEs)
    }
  }
  # remove all events after death
  for (i in 1:length(AEs)) {
    info <- AEs[[i]]$info
    new_start <- info$Start[info$Start < min_death_time]
    new_severities <- info$Severity[info$Start < min_death_time]
    new_end <- info$End[info$End < min_death_time]

    if (length(new_start) > length(new_end)) {
      new_end <- c(new_end, min_death_time)
    }
    new_start <- c(new_start, min_death_time)
    new_severities <- c(new_severities, DEATH)
    new_end <- c(new_end, AEs[[i]]$max_time)
    new_duration <- new_end - new_start
    AEs[[i]]$info <- data.frame(Start = new_start, End = new_end, Severity = new_severities, Duration = new_duration)
    AEs[[i]]$score <- sum((new_end - new_start) * SEVERETY_WEIGHTS[new_severities])
  }

  if (AEs_are_element) {
    simulated_AEs$AEs <- AEs
    if ("score" %in% names(simulated_AEs)) { # recompute the score
      simulated_AEs$score_without_death <- simulated_AEs$score
      simulated_AEs$score <- sum(sapply(AEs, function(AE) AE$score))
    }
    return(simulated_AEs)
  }
  else {
    return(AEs)
  }
}


set_death_trial_list <- function(trials) {
  for (i in 1:length(trials)) {
    treatment_group <- trials[[i]]$treatment
    control_group <- trials[[i]]$control
    for (t_p_i in 1:length(treatment_group)) {
      trials[[i]]$treatment[[t_p_i]] <- set_death(treatment_group[[t_p_i]])
    }
    for (c_p_o in  1:length(control_group)) {
      trials[[i]]$control[[c_p_o]] <- set_death(control_group[[c_p_o]])
    }
  }
  return(trials)
}

