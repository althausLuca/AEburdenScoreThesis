# file containing the functions to simulate adverse events up to a single list of events
# source("R/AdverseEvents/death_case_computations.R")
source("R/data_generation/AE_simulation/AdverseEvent.R")

#' Simulate a single event
#'
#' This function simulates an event based on the provided event parameters.
#'
#' @param A_type
#' @param max_time Maximum time to simulate events (in days)
#' @param surpress_info A boolean indicating whether to compute the info data frame (default = FALSE).
#' If TRUE, the info data frame is not computed and returned (saves a lot of time)
#'
#' @return An event object with simulated event times , severities and durations
#'
#' @seealso \code{\link{AE}} for defining an event
simulate_event <- function(AE_type, max_time, susceptibility=1, surpress_info = FALSE) {
  AE_type <- apply_susceptibility(AE_type, susceptibility)

  stopifnot(max_time > 0)

  starting_times <- numeric(0) #starting times
  ending_times <- numeric(0) #ending times
  severities <- numeric(0)

  score <- 0

  event_start <- sample_to_first_event(AE_type)

  while (event_start < max_time) {
    starting_times <- c(starting_times, event_start)

    event_duration <- sample_event_duration(AE_type)
    event_duration <- min(max_time - event_start , event_duration)

    event_end <- event_start + event_duration
    ending_times <- c(ending_times, event_end)

    severity <- sample_severity(AE_type)

    score <- score + event_duration * severity

    time_between_events <- sample_time_gap_time(AE_type)

    severities  <- c(severities, severity)

    event_start <- event_end + time_between_events
  }

  n_events <- length(severities)

  stopifnot(length(ending_times) == length(severities))
  stopifnot(length(starting_times) == length(ending_times))
  stopifnot(length(ending_times) == 0 || max(ending_times) <= max_time)

  event <- list()

  if (!surpress_info) { # do not compute the info data frame if surpressed (saves a lot of time)
    info <- data.frame(
      Severity = severities, # Severity of each AE
      Start = starting_times, # Start position of each AE
      End = ending_times, # End position is Start position plus Duration
      Duration = ending_times - starting_times # Duration of each AE
    )
    event$info <- info
  }

  event$n_events <- n_events
  event$max_time <- max_time
  event$score <- score

  stopifnot(event$score >= 0)

  return(event)
}

#' Simulate a set of events
#' This function simulates multiple events based on the provided event parameters.
#' @param a list of  AE objects
#' @param max_time Maximum time to simulate events (in days)
#' @param death A boolean indicating whether to simulate death events (default = FALSE)
#' @param susceptibility A list of susceptibility values for the group
#' @param surpress_info A boolean indicating whether to compute the info data frame
#' @return A list of the simualted AEs, the toal score and the number of events
simulate_events <- function(AEs, max_time, death = FALSE, susceptibility = 1, surpress_info = FALSE) {
  simulated_AEs <- list()

  for (i in 1:length(AEs)) {
    AE <- AEs[[i]]
    AE <- simulate_event(AE, max_time ,susceptibility=susceptibility, surpress_info = surpress_info)
    simulated_AEs[[i]] <- AE
  }

  if (death) {
    simulated_AEs <- set_death(simulated_AEs)
  }

  scores <- sapply(simulated_AEs, function(x) x$score)
  n_events <- sapply(simulated_AEs, function(x) x$n_events)

  results <- list(
    AEs = simulated_AEs,
    score = sum(scores),
    n_events = sum(n_events)
  )
  return(results)
}


