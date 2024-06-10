print("test")

parse_input <- function(){

  args <- commandArgs(trailingOnly = TRUE)

  # Check if the start argument is provided
  if (length(args) < 1) {
    stop("Please provide the start argument.")
  }

  # Convert the first argument to a numeric value
  input <- as.numeric(args[1])

  # Check if the conversion was successful
  if (is.na(input)) {
    stop("The start argument must be a numeric value.")
  }
  print("input given:")
  print(input)
  return(input)
}

# Install the gamlss package if not already installed
if (!require(gamlss)) {
  install.packages("gamlss")
  print("gamlss installed")
}

# Load the gamlss package
library(gamlss)

# Load the data
load("../../../inflated_gamma.RData")

# Initialize the list to store trial mu estimates

trial_mu_estimates <- list()
start <- parse_input()
step <- 5


trial_indices <- (start+1):(start+step)
# Loop through the trials
for(trial_index in trial_indices){
  trial <- trial_data$trials[[trial_index]]
  print(trial_index)
  trial_mu_estimates[[trial_index-start]] <- lapply(1:n_sim , function(i) get_mean_estimate(get_permuted_trial(trial)))
  save.image(file = paste0("inflated_gamma_", start , ".RData"))
}






