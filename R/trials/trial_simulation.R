source("R/AdverseEvents/event_simulation.R")

#' Simulate a trial group with a given set of AEs and specified susceptibility parameters
#' @param AEs A list of Adverse Events to simulate
#' @param susceptibility_parameter A list of parameters for the susceptibility distribution
#' (e.g. c("constant") ,c("gamma", 1.5))
#' @param max_time Maximum time to simulate events (in days)
#' @stores the trial data to a csv file in the result_path with the scenario name
simulate_group <- function(AEs, size = 100, susceptibility_parameter = list("gamma", 1.5), max_time = 180) {
  # convert AEs to list if they are not
  if (!is.list(AEs[[1]])) {
    AEs <- lapply(1:nrow(AEs), function(i) as.list(AEs[i,]))
  }

  susceptibility_type <- susceptibility_parameter[[1]]
  if (susceptibility_type == "constant") {
    susceptibility <- rep(1, size)
  } else if (susceptibility_type == "binary") {
    unif_rvs <- runif(size, min = 0, max = 1)
    susceptibility <- (unif_rvs > susceptibility_parameter[2]) * 1
  } else {
    theta <- susceptibility_parameter[[2]]
    susceptibility <- 1 / rgamma(size, shape = theta, scale = 1 / theta)
  }


  results <- matrix(nrow = size, ncol = 2) # Preallocate a matrix for scores and n_events
  colnames(results) <- c("Score", "N_Events")

  for (i in seq_along(susceptibility)) {
    simulated_events <- simulate_events(AEs, max_time, death = FALSE, susceptibility = susceptibility[i], surpress_info = TRUE)
    results[i, "Score"] <- simulated_events$score
    results[i, "N_Events"] <- simulated_events$n_events
  }

  # If you need them as separate vectors
  scores <- results[, "Score"]
  n_events <- results[, "N_Events"]

  results <- list(
    scores = scores,
    susceptibility = susceptibility,
    susceptibility_parameter = paste0(susceptibility_parameter, collapse = "_"),
    AEs = AEs,
    n_events = n_events
  )
  return(results)
}

#' Simulate a scenario
#' This function simulates a scenario
#' @param scenario A  scenario @seealso \code{\link{R/Scenarios.R}}
#' @param n_sim The number of simulations to run (default = 1000)
#' @param susceptibility_parameter A list of parameters for the susceptibility distribution
#' (e.g. c("constant") ,c("gamma", 1.5))
#' @param death A boolean indicating whether to simulate death events (default = FALSE)
#' @param save A boolean indicating whether to save the results to a RDS file (default = TRUE)
simulate_trials_from_scenario <- function(scenario, n_sim = 5000,
                                          susceptibility_parameter = list("gamma", 1.5),
                                          death = FALSE,
                                          save = TRUE,
                                          file_name = NULL, trial_group_size = 100 , result_path = "data/trials/") {
  print("generating scores")
  if (is.null(file_name)) {
    file_name <- scenario$name
  }
  file_path <- paste0(result_path, file_name, ".csv")


  treatment_AEs <- scenario$treatment
  control_AEs <- scenario$control

  # Open CSV file in append mode
  file.remove(file_path, showWarnings = FALSE)

  if (!dir.exists(dirname(file_path))) {
    dir.create(dirname(file_path), recursive = TRUE)
  }

  file_conn <- file(file_path, "a")

  # Loop through simulations
  for (index in 1:n_sim) {
    # Simulate control and treatment scores
    control_scores <- simulate_group(control_AEs, size = trial_group_size, susceptibility_parameter = susceptibility_parameter, max_time = 180)$scores
    treatment_scores <- simulate_group(treatment_AEs, size = trial_group_size, susceptibility_parameter = susceptibility_parameter, max_time = 180)$scores

    # Write results to CSV file as one row
    write.table(as.data.frame(rbind(control_scores, treatment_scores), row.names = c("control", "treatment")),
                file = file_conn, sep = ",", col.names = FALSE, row.names = TRUE, append = TRUE)

    # Print progress
    if (index %% 20 == 0) {
      print(index / n_sim)
    }
  }
  print(close(file_conn))

  return(file_name)
}
