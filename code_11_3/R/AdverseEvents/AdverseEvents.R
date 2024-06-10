library(tibble)
source("R/scenario_configuration.R")

#' Define an Adverse Event
#'
#' This function defines an event with parameters such as time to the first event,
#' event duration, time between events, event probability, and severity probabilities.
#'
#' @param time_to_first_event Expected time until the first event (in days)
#' @param event_duration Expected duration of each event (in days)
#' @param time_between_events Expected time between events (in days )
#' @param event_prob Probability of an event happening at all (0 to 1)
#' @param severity_probability Probability distribution for event severity (vector of probabilities)
#'
#' @return An event object with defined parameters
AE <- function(event_duration, time_between_events,
               severity_probability = SEVERITY_PROBABILITIES$equal,
               time_to_first_event = time_between_events) {
  event <- list(
    time_to_first_event = time_to_first_event,
    duration = event_duration,
    time_between_events = time_between_events,
    gap_time = time_between_events,
    event_prob = 1,
    severity_probability = severity_probability
  )
  # set class of event to AE
  class(event) <- "AE"
  return(event)
}

str.AE <- function(x, ...) {
  return(paste0("AE(", x$time_to_first_event, ",", x$event_duration, ",", x$time_between_events,
                ",c(", round(x$severity_probability[1], 3),
                ",", round(x$severity_probability[2], 3),
                ",", round(x$severity_probability[3], 3), ")",
                ")"))
}










