source("R/trials/trial_simulation.R")
source("R/methods/run_methods.R")

trials_file <- "Scenario_2_k_1.5_s_0.5.csv"

trial_data <- load_trial_data(trials_file)
trial_data$n_trials

xi_values <- c(1.1, 1.7, 2)
link_power <- 1

results_df <- data.frame(
  xi = numeric(),
  trial_id = numeric(),
  p_value = numeric(),
  estimate = numeric(),
  stringsAsFactors = FALSE
)

profile_results_df <- data.frame(
  xi = numeric(),
  trial_id = numeric(),
  p_value = numeric(),
  estimate = numeric(),
  stringsAsFactors = FALSE
)
profile_result <- "for debuging"
counter <- 0
for (trial in trial_data$trials) {
  if (counter > 50){
    break
  }
  trial_id <- counter
  counter <- counter+1
  for (xi_value in xi_values) {
    model_result <- run_tweedie(trial, var.power = xi_value, link.power = link_power)

    results_df <- rbind(results_df, data.frame(
      xi = xi_value,
      trial_id = trial_id,
      p_value = model_result$p_value,
      estimate = model_result$estimate
    ))

  }

  profile_result <<- tweedie.profile(trial$Score ~ trial$Group, link.power = link_power,fit.glm=F , xi.vec
  = seq(1.1, 2, by=0.1) )
  optimal_xi <- profile_result$xi.max

  print("optimal xi")
  print(optimal_xi)
  model_result <- run_tweedie(trial, var.power = optimal_xi, link.power = link_power)

  profile_results_df <- rbind(profile_results_df ,data.frame(
    xi = optimal_xi,
    trial_id = trial_id,
    p_value = model_result$p_value,
    estimate = model_result$estimate
  ))
}


library(ggplot2)

# Filter the results for a specific xi value
specific_xi <- 1.5

# Order p_values
p_value <- profile_results_df$p_value[order(profile_results_df$p_value)]
plot(p_value)
for(specific_xi in xi_values){
  filtered_data <- results_df[results_df$xi == specific_xi,]$p_value
  p_values <- filtered_data[order(filtered_data)]
  lines(p_values)
}

profile_results_df$xi <- profile_results_df$xi[order(profile_results_df$xi)]
plot(profile_results_df$xi)
significant_indices <- seq_along(profile_results_df$p_value)[profile_results_df$p_value < 0.05]
points(significant_indices, profile_results_df$xi[significant_indices] , col = "red")







