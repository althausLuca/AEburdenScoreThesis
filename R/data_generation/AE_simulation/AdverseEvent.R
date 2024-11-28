#' Define an Adverse Event (Type)
#'
#' This function defines an event with parameters such as time to the first event,
#' event duration, time between events, event probability, and severity probabilities.
#'
#' @param time_to_first_event Expected time until the first event (in days)
#' @param event_duration Expected duration of each event (in days)
#' @param time_between_events Expected time between events (in days )
#' @param severity_probabilities Probability distribution for event severity (vector of probabilities)
#'
#' @return An event object with defined parameters
AE <- function(event_duration,
               time_between_events,
               severity_probabilities,
               severity_weights = c(1, 2, 3),
               time_to_first_event = time_between_events) {

  event <- list(
    time_to_first_event = time_to_first_event,
    duration = event_duration,
    gap_time = time_between_events,
    severity_weights = severity_weights,
    severity_probabilities = severity_probabilities
  )
  # set class of event to AE
  class(event) <- "AE_type"
  check_AE(event)
  return(event)
}


check_AE <- function(AE_type) {
  with(AE_type, {
    stopifnot(time_to_first_event > 0)
    stopifnot(duration > 0)
    stopifnot(gap_time > 0)
    stopifnot(length(severity_probabilities) > 0)
    stopifnot(sum(severity_probabilities) == 1)
    stopifnot(length(severity_weights) == length(severity_probabilities))
  })
}


apply_susceptibility <- function(AE_type, susceptibility) {
  stopifnot(is.null(AE_type$susceptibility))
  check_AE(AE_type)

  AE_type$time_to_first_event <- AE_type$time_to_first_event / susceptibility
  AE_type$gap_time <- AE_type$gap_time / susceptibility
  AE_type$susceptibility <- susceptibility

  check_AE(AE_type)
  return(AE_type)
}


##sampling methods for an adverse event

sample_to_first_event <- function(AE_type, n = 1) {
  check_AE(AE_type)
  if (AE_type$gap_time == Inf) { return(Inf) }
  return(rexp(n, 1 / AE_type$gap_time))
}

sample_event_duration <- function(AE_type, shape = 9, n = 1) {
  if (AE_type$duration == Inf) { return(Inf) }
  return(rgamma(n, shape = shape, scale = AE_type$duration / shape))
}

sample_time_gap_time <- function(AE_type, n=1 , s = rep(1, n)) {
  if (AE_type$gap_time == Inf) { return(Inf) }
  return(rexp(length(s), s / AE_type$gap_time))
}

sample_severity <- function(AE_type, n = 1) {
  return(sample(AE_type$severity_weights, size = n, prob = AE_type$severity_probabilities, replace = TRUE))
}


change_AE_type_property <- function(AE_type, property, factor) {
  if (!is.null(AE_type[[property]])) {
    AE_type[[property]] <- AE_type[[property]] * factor
    check_AE(AE_type)
    return(AE_type)
  }
  #list of AE_types
  if (!is.null(AE_type[[1]][[property]])) {
    return(lapply(AE_type, function(AE) { change_AE_type_property(AE, property, factor) }))
  }
  stop("Property not found")
}


