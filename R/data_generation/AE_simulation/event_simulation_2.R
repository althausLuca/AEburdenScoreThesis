# file containing the functions to simulate adverse events up to a single list of events
# This method can genereat emultiple outputs for the same AE type at once by operating on a single dataframe
source("R/data_generation/AE_simulation/AdverseEvent.R")
source("R/data_generation/AE_types.R")
source("R/data_generation/AE_simulation/event_simulation.R")


#' Simulate a AE Type event n times
#' This function simulates an event based on the provided event parameters.
#' @param AE_type
#' @param max_time Maximum time to simulate events (in days)
#' @param n Number of events to simulate
#' @param k_d Shape parameter for the event duration distribution
#' @param k_s (inverse) Shape parameter for the susceptibility distribution
#' @param susceptibility A vector of susceptibility values for the group (default = 1 / rgamma(n, shape = k_s, scale = 1 / k_s))
#' @return A data frame with columns score, n_events and duration.
#' Where score is the total score, n_events is the number of events and duration is the total duration of the events
simulate_event <- function(AE_type,
                           max_time = 180,
                           n = 1,
                           k_d = 9,
                           k_s = 1.5,
                           susceptibility = 1 / rgamma(n, shape = k_s, scale = 1 / k_s)) {
  s <- susceptibility
  n <- length(susceptibility)
  result <- data.frame(
    t = rep(0, n),
    score = rep(0, n),
    n_events = rep(0, n),
    duration = rep(0, n)
  )

  i <- which(result$t < max_time)
  while (length(i) > 0) {
    result$t[i] <- result$t[i] + sample_time_gap_time(AE_type, s = c(s[i]))
    i <- which(result$t < max_time)
    if (length(i) == 0) { break } # break if no more events can be simulated (faster and avoids warnings)
    result$n_events[i] <- result$n_events[i] + 1
    event_duration <-  sample_event_duration(AE_type, n = length(i), shape = k_d)
    event_duration <- ifelse(event_duration < (max_time - result$t[i]), event_duration, max_time - result$t[i])
    new_scores <- event_duration * sample_severity(AE_type, n = length(i))
    result$score[i] <- result$score[i] + new_scores
    result$duration[i] <- result$duration[i] + event_duration
    result$t[i] <- result$t[i] + event_duration
  }

  return(result)
}

#' Simulate a set of events
#' This function simulates multiple events based on the provided event parameters.
#' @param a list of  AE objects
#' @param max_time Maximum time to simulate events (in days)
#' @param n Number of events to simulate
#' @param k_d Shape parameter for the event duration distribution
#' @param k_s (inverse) Shape parameter for the susceptibility distribution
#' @param susceptibility A of susceptibility values (default = 1 / rgamma(n, shape = k_s, scale = 1 / k_s) , n and k_s are not used if susceptibility is provided)
#' @return A dataframe with columns score, n_events and duration.
simulate_events <- function(AE_types,
                            max_time = 180,
                            n = 1,
                            k_d = 9,
                            k_s = 1.5,
                            susceptibility = 1 / rgamma(n, shape = k_s, scale = 1 / k_s)) {
  result <- NULL
  for (i in 1:length(AE_types)) {
    res <- simulate_event(AE_types[[i]], max_time, n, k_d, k_s, susceptibility)
    if (is.null(result)) {
      result <- res
    } else {
      result <- result + res
    }
  }
  return(result)
}

## Example usage
source("R/data_generation/AE_types.R")
Long <- 1500
AE1 <- AE(3, Long, MOSTLY_MILD)
AE2 <- AE(7, Long, MOSTLY_MODERATE)
AE3 <- AE(3, Long / 2, MOSTLY_MILD)
AEs <- list(AE1, AE2, AE3)

k_s <- 1.5
k_d <- 9

res <- simulate_events(AEs, max_time = 180, n = 500000, k_d = k_d , k_s = k_s)
mean(1-(res$score!=0))
mean(res$score)
mean(res$score[res$score!=0])
sd(res$score)
summary(res$score[res$score!=0])
sort(res$duration[res$score!=0]/res$n_events[res$score!=0])[1:3]
max(res$duration)

sum(res$score[res$score!=0] < 0.2)/500000