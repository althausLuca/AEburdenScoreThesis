source("R/scenario_configuration.R")
source("R/AdverseEvents/death_case_computations.R")
# library(tibble)

#' Simulate a single event
#'
#' This function simulates an event based on the provided event parameters.
#'
#' @param event An event object
#' @param max_time Maximum time to simulate events (in days)
#'
#' @return An event object with simulated event times
#'
#' @seealso \code{\link{AE}} for defining an event
simulate_event <- function(event, max_time ,susceptibility = 1) {
  start <- numeric(0)
  end <- numeric(0)
  score <- 0

  ## extract params
  expected_time_to_first_event <- event$time_to_first_event
  expected_event_duration <- event$event_duration
  expected_time_between_events <- event$time_between_events
  severity_probabilies <- event$severity_probability

  ## apply subscesibility
  expected_time_to_first_event <- expected_time_to_first_event / susceptibility
  #expected_event_duration <- expected_event_duration * subsceptibility
  expected_time_between_events <- expected_time_between_events / susceptibility

  time_to_first_event <- get_time_to_first_event(expected_time_to_first_event)
  event_start <- time_to_first_event

  while (event_start < max_time) {
    start <- c(start, event_start)

    event_duration <- get_event_duration(expected_event_duration)
    event_stop <- event_start + event_duration
    end <- c(end, min(event_stop, max_time))

    time_between_events <- get_time_between_events(expected_time_between_events)
    event_start <- event_stop + time_between_events
  }


  n_events <- length(start)
  severities <- sample(SEVERITIES, size = n_events, replace = TRUE, prob = severity_probabilies)

  stopifnot(length(start) == length(end) && length(end) == length(severities))
  stopifnot(length(end)==0 || max(end) <= max_time)

  info <- data.frame(
    Severity = severities, # Severity of each AE
    Start = start, # Start position of each AE
    End = end, # End position is Start position plus Duration
    Duration = end - start # Duration of each AE
  )

  event$info <- info
  event$n_events <- n_events
  event$max_time <- max_time
  event$score <- sum((info$End - info$Start) * SEVERETY_WEIGHTS[info$Severity])

  return(event)
}


get_time_to_first_event <- function(mean_time) {
  if (mean_time == Inf) {
    return(Inf)
  }
  return(rexp(1, 1/mean_time))
}

get_event_duration <- function(mean_duration) {
  if (mean_duration == Inf) {
    return(Inf)
  }
  return(rexp(1, 1/mean_duration))
}

get_time_between_events <- function(mean_time) {
  if (mean_time == Inf) {
    return(Inf)
  }
  return(rexp(1, 1/mean_time))
}


#' Simulate a single set of events
#' This function simulates multiple events based on the provided event parameters.
#' @param one AE or a list of  of AE objects
simulate_events <- function(AEs, max_time, death = FALSE , susceptibility = 1) {
  simulated_AEs <- list()
  print(susceptibility)
  for (i in 1:length(AEs)) {
    AE <- AEs[[i]]
    AE <- simulate_event(AE, max_time , susceptibility= susceptibility)
    simulated_AEs[[i]] <- AE
  }

  if (death) {
    simulated_AEs <- set_death(simulated_AEs)
  }

  scores <- sapply(simulated_AEs, function(x) x$score)

  results <- list(
    AEs = simulated_AEs,
    score = sum(scores)
  )
  return(results)
}

