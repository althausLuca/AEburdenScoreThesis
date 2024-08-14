library(gamlss)
source("R/trials/trial_loader.R")
trial_data <- load_equal_trials()


gamlss_model_ <- NULL
gamlss_p_values <- function(mu.link = "inverse", data = trial, nu.formula = ~Group) {
  on.exit(rm(non_zero_data_global__, envir = .GlobalEnv), add = TRUE)
  gamlss_family <- do.call(GA, list(mu.link = mu.link))
  non_zero_data_global__ <<- trial[trial$Score > 0,] #somewhere in the gammlls library they need to eval/parsed this from the global environment
  gamlss_model <- gamlss(Score ~ Group, sigma.formula =  ~Group, family = gamlss_family, data = non_zero_data_global__)
  gamlss_model_ <<- gamlss_model
  treatment_p_values <- summary(gamlss_model_)[2, 4]
  return(treatment_p_values)
}




link_functions <- c("inverse", "log", "identity")

results <- data.frame(
  trial = numeric(),
  link_function = character(),
  treatment_p_value = numeric()
)


n_sim <- 2000

trial_s <- seq(1, trial_data$n_trials, by = 1)

for (i in trial_s) {
  # if (i < n_sim) { next }

  trial <- trial_data$trials[[i]]
  # trial.2 <- trial_data$trials[[i + 1]]

  # trial[trial$Group == "treatment",]$Score <- trial.2[trial.2$Group == "control",]$Score

  for (link_function in link_functions) {
      treatment_p_value <- gamlss_p_values(mu.link = link_function, data = trial)
      results <- rbind(results,
                       data.frame(trial = i,
                                  link_function = link_function,
                                  treatment_p_value = treatment_p_value))
  }
}


# print rate of significant p_values at 0.05 level
results_sig <- results[, 3] < 0.05
for(link_f in link_functions) {
  link_results <- results_sig[results$link_function == link_f]
  print(paste(link_f, sum(link_results/length(link_results))))
}






