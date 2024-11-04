source("R/data_generation/AE_simulation/trial_simulation.R")

init_trial_data <- function(
  control_AE_types,
  treatment_AE_types,
  n_participants = 100,
  max_time = 180,
  susceptibility_parameter = 1.5,
  n_trials = 5000,
  file_path = "data/trials/trials.RData", recompute = FALSE) {

  if (!recompute && !is.null(loaded_trial_data <- load.trial_data(file_path))) {
    print(paste0("Trial data already exists: ", file_path))
    if (loaded_trial_data$n_trials == n_trials &&
      loaded_trial_data$n_participants == n_participants &&
      loaded_trial_data$max_time == max_time &&
      loaded_trial_data$susceptibility_parameter == susceptibility_parameter &&
      identical(loaded_trial_data$AEs$control, control_AE_types)) {
      return(loaded_trial_data)
    }
    print(paste0("Trial data is different ... recomputing"))
  }


  trial_seeds <- sample(10^9, n_trials, replace = FALSE)

  trial_data <- new.env()
  class(trial_data) <- "trial_data"

  trial_data$n_trials <- n_trials
  trial_data$AEs <- list(control = control_AE_types,
                         treatment = treatment_AE_types)

  trial_data$trial_seeds <- trial_seeds
  trial_data$n_participants <- n_participants
  trial_data$max_time <- max_time
  trial_data$susceptibility_parameter <- susceptibility_parameter
  trial_data$file_path <- file_path

  save.trial_data(trial_data)
  return(trial_data)
}

save.trial_data <- function(trial_data, file_path = trial_data$file_path) {
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  base::save(trial_data, file = file_path)
}

load.trial_data <- function(file_path) {
  if (!file.exists(file_path)) {
    return(NULL)
  }
  base::load(file_path, env = tmp <- new.env())
  return(tmp$trial_data)
}

simulate_trial_data <- function(trial_data, save = T, recompute = F) {

  if (!recompute && "trials" %in% names(trial_data)) {
    print(paste0("Trial data already exists: ", trial_data$file_path))
    return(trial_data)
  }

  n_participants <- trial_data$n_participants
  max_time <- trial_data$max_time
  susceptibility_parameter <- trial_data$susceptibility_parameter
  trial_seeds <- trial_data$trial_seeds
  control_AE_types <- trial_data$AEs$control
  treatment_AE_types <- trial_data$AEs$treatment

  trials <- vector("list", trial_data$n_trials)

  for (trial_i in 1:trial_data$n_trials) {
    seed <- trial_seeds[trial_i]
    set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
    control_scores <- simulate_group(control_AE_types, size = n_participants, susceptibility_parameter = list("gamma", susceptibility_parameter), max_time = max_time)$scores
    treatment_scores <- simulate_group(treatment_AE_types, size = n_participants, susceptibility_parameter = list("gamma", susceptibility_parameter), max_time = max_time)$scores
    trial <- list(control = unlist(control_scores), treatment = unlist(treatment_scores))
    trial_data$trials[[trial_i]] <- trial
  }
  trial_data$trials
  if (save) {
    save.trial_data(trial_data)
  }

  return(trial_data)
}

get_control_scores <- function(trial_data) {
  return(c(sapply(trial_data$trials, function(x) x$control, simplify = TRUE)))
}

get_treatment_scores <- function(trial_data) {
  return(c(sapply(trial_data$trials, function(x) x$treatment)))
}

summary.trial_data <- function(trial_data) {
  control_scores <- get_control_scores(trial_data)
  treatment_scores <- get_treatment_scores(trial_data)

  df <- data.frame(mean = c(mean(control_scores), mean(treatment_scores)),
                   sd = c(sd(control_scores), sd(treatment_scores)),
                   median = c(median(control_scores), median(treatment_scores)),
                   zero = c(mean(control_scores == 0), mean(treatment_scores == 0)))

  row.names(df) <- c("control", "treatment")

  return(df)
}

trial_as_df <- function(trial) {
  df_ <- data.frame(Score = c(trial$control, trial$treatment),
                    Group = c(rep("control", length(trial$control)), rep("treatment", length(trial$treatment)))
  )
  return(df_)
}

apply_to_trials <- function(trial_data, func, return_df = FALSE, limit = NULL, trials_as_df = TRUE, ...) {
  # Determine the number of trials to process
  n <- if (is.null(limit)) length(trial_data$trials) else min(limit, length(trial_data$trials))
  cat("\n")

  func_silent <- function(i, trial, ...) {
    success <- FALSE
    on.exit({ if (!success) sink() })
  {
    # result <- func(trial, ...)
    sink("/dev/null"); ; result <- func(trial, ...); sink();
  }
    cat("\r Processing trial ", i, " of ", n, " trials")
    success <- TRUE
    return(result)
  }


  # Initialize an empty list to store results
  results <- vector("list", n)

  # Loop over the trials and apply the function

  for (i in seq_len(n)) {
    results[[i]] <- func_silent(i, trial_as_df(trial_data$trials[[i]]), ...)
  }

  # Combine results into a data frame if as.df is TRUE
  if (return_df) {
    results <- data.frame(do.call(rbind, results))
  }
  return(results)
}

check_if_trial_data_is_equal <- function(trial_data_1, trial_data_2) {
  if (trial_data_1$n_trials != trial_data_2$n_trials) {
    return(FALSE)
  }
  if (trial_data_1$n_participants != trial_data_2$n_participants) {
    return(FALSE)
  }
  if (trial_data_1$max_time != trial_data_2$max_time) {
    return(FALSE)
  }
  if (trial_data_1$susceptibility_parameter != trial_data_2$susceptibility_parameter) {
    return(FALSE)
  }
  if (!identical(trial_data_1$AEs$control, trial_data_2$AEs$control)) {
    return(FALSE)
  }
  return(TRUE)
}