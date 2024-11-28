data_generating_mechanism <- function(
  control_AE_types,
  treatment_AE_types,
  n_participants = 100,
  trial_duration = 180, #T
  susceptibility_parameter = 1.5, #k_s
  duration_shape = 9 #k_d
) {

  result <- new.env()
  class(result) <- "data_generating_mechanism"

  result$n_participants <- n_participants
  result$trial_duration <- trial_duration
  result$susceptibility_parameter <- susceptibility_parameter
  result$duration_shape <- duration_shape
  result$AEs <- list(control = control_AE_types,
                     treatment = treatment_AE_types)
  return(result)
}

# simulate.data_generating_mechanism <- function(data_generating_mechanism, save = T, recompute = F) {


source("R/data_generation/AE_types.R")
dgm <- data_generating_mechanism(DEFAULT_AE_TYPES, DEFAULT_AE_TYPES)

if (!recompute && "data_generating_mechanism" %in% names(data_generating_mechanism)) {
  print(paste0("Data generating mechanism already exists: ", data_generating_mechanism$file_path))
  return(data_generating_mechanism)
}

replicate(5, simulate_trial_data(dgm))

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
#   return(trial_data)
# }
