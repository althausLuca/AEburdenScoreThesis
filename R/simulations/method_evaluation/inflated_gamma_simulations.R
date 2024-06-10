install.packages("gamlss")
library(gamlss)

source("R/trials/trial_simulation.R")
source("R/simulations/method_evaluation/zero_inflated_gamma_functions.R")




file <- "Scenario_2_k_1.5_s_0.5.csv"

# Load trial data
trial_data <- load_trial_data(file)

n_sim <- 10000

trial <- trial_data$trials[[1]]
trial.p <- get_permuted_trial(trial)


trial_mu_estimates <- list()

for(trial_index in 1:100){
  trial <- trial_data$trials[[1]]
  trial_mu_estimates[[trial_index]] <- lapply(1:n_sim , function(i) get_mean_estimate(get_permuted_trial(trial)))
  save.image(file = "inflated_gamma.RData")
}







