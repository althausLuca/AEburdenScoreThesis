# file containing the functions to simulate adverse events up to a single list of events
source("R/AdverseEvents/death_case_computations.R")



#' Simulate a single event
#'
#' This function simulates an event based on the provided event parameters.
#'
#' @param event_settings A list of event parameters:
#' - event_duration: Expected duration of each event (in days)
#' - gap_time: Expected time between events (in days )
#' - time_to_first_event: Expected time until the first event (in days) if not provided,
#' it is set to the same as gap_time
#' @param max_time Maximum time to simulate events (in days)
#' @param susceptibility parameter to adjust the gap time
#' @param surpress_info A boolean indicating whether to compute the info data frame (default = FALSE).
#' If TRUE, the info data frame is not computed and returned (saves a lot of time)
#'
#' @return An event object with simulated event times
#'
#' @seealso \code{\link{AE}} for defining an event
simulate_event <- function(event_settings, max_time, susceptibility = 1, surpress_info = FALSE) {
  stopifnot(max_time > 0)
  stopifnot("severity" %in% names(event_settings))

  if (length(missing_names <- setdiff(c("duration", "gap_time"), names(event_settings))) > 0)
    stop("Missing: ", paste(missing_names, collapse = ", "), ", got:", paste(event_settings, collapse = ", "))


  start <- numeric(0)
  end <- numeric(0)

  score <- 0

  ## extract parameters
  expected_event_duration <- event_settings$duration
  expected_time_between_events <- event_settings$gap_time
  expected_time_to_first_event <- ifelse("time_to_first_event" %in% names(event_settings),
                                         event_settings$time_to_first_event,
                                         expected_time_between_events)

  severity_probabilies <- unlist(event_settings$severity)

  ## apply subscesibility
  expected_time_to_first_event <- expected_time_to_first_event / susceptibility
  expected_time_between_events <- expected_time_between_events / susceptibility
  # expected_time_between_events =  1/lambda

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

  stopifnot(length(end) == length(severities))
  stopifnot(length(start) == length(end))
  stopifnot(length(end) == 0 || max(end) <= max_time)

  event <- list()

  if (!surpress_info) { # do not compute the info data frame if surpressed (saves a lot of time)
    info <- data.frame(
      Severity = severities, # Severity of each AE
      Start = start, # Start position of each AE
      End = end, # End position is Start position plus Duration
      Duration = end - start # Duration of each AE
    )
    event$info <- info
  }

  event$n_events <- n_events
  event$max_time <- max_time
  event$score <- sum((end - start) * SEVERETY_WEIGHTS[severities])  #sum((info$End - info$Start) * SEVERETY_WEIGHTS[info$Severity])

  stopifnot(event$score >= 0)

  return(event)
}

get_time_to_first_event <- function(mean_time) {
  if (mean_time == Inf) {
    return(Inf)
  }
  return(rexp(1, 1 / mean_time))
}

get_event_duration <- function(mean_duration) {
  if (mean_duration == Inf) {
    return(Inf)
  }
  # ceiling(rexp(1, 1 / mean_duration))
  #
  shape <- ifelse(exists("duartion_shape_"), duartion_shape_, 9)
  print(shape)
  result <- rgamma(1,shape=shape, scale=mean_duration/shape) #rexp(1, 1 / mean_duration)
  return(result)
}

get_time_between_events <- function(mean_time) {
  if (mean_time == Inf) {
    return(Inf)
  }
  return(rexp(1, 1 / mean_time))
}


time_between_events_generator <- function(mean_time) {
  counter <- 0
  varbiables <- ifelse(mean_time == Inf, c(Inf), rexp(100, 1 / mean_time))
  function() {
    counter <<- counter + 1
    if (counter > length(varbiables)) {
      counter <<- 0
      varbiables <<- rexp(100, 1 / mean_time)
    }
    return(varbiables[counter])
  }
}


#' Simulate a single set of events
#' This function simulates multiple events based on the provided event parameters.
#' @param a list of  AE objects
#' @param max_time Maximum time to simulate events (in days)
#' @param death A boolean indicating whether to simulate death events (default = FALSE)
#' @param susceptibility A list of susceptibility values for the group
#' @param surpress_info A boolean indicating whether to compute the info data frame
#' @return A list of scores and susceptibility values for the group
simulate_events <- function(AEs, max_time, death = FALSE, susceptibility = 1, surpress_info = FALSE) {
  simulated_AEs <- list()
  for (i in 1:length(AEs)) {
    AE <- AEs[[i]]
    AE <- simulate_event(AE, max_time, susceptibility = susceptibility , surpress_info = surpress_info)
    simulated_AEs[[i]] <- AE
  }

  if (death) {
    simulated_AEs <- set_death(simulated_AEs)
  }

  scores <- sapply(simulated_AEs, function(x) x$score)
  n_events <- sapply(simulated_AEs, function(x) x$n_events)
  # print(scores)
  results <- list(
    AEs = simulated_AEs,
    score = sum(scores),
    n_events = sum(n_events)
  )
  return(results)
}


