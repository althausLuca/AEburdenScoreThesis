source("R/trials/trial_simulation.R")
source("R/methods/run_methods.R")

trials_file <- "Scenario_2_k_1.5_s_0.5.csv"

trial_data <- load_trial_data(trials_file)
trial_data$n_trials

results_df <- data.frame(
  c = numeric(),
  trial_id = numeric(),
  p_value = numeric(),
  estimate = numeric(),
  stringsAsFactors = FALSE
)

c_values <- c(0.00001,0.001,1,10,100,1000,10000,100000,1000000,10000000)

counter <- 0
for (trial in trial_data$trials) {
  counter <- counter+1
  #if (counter < 500){
  #  next
  #}
  trial_id <- counter
  counter <- counter+1
  for (c in c_values) {
    model_result <- run_anova(trial,delta=c)

    results_df <- rbind(results_df, data.frame(
      c = c,
      trial_id = trial_id,
      p_value = model_result$p_value,
      estimate = model_result$estimate
    ))

  }
}


# Order p_values

for( specific_c in  c(0.00001,0.001,1,10,100,1000,10000,100000,1000000,10000000)){
  p_value <- results_df$p_value[results_df$c == specific_c]
  print(paste("c:",specific_c,"p_value<0.05:",sum(p_value<0.05)/length(p_value) ))
}

p_value <- results_df$p_value

plot(factor(results_df$c), p_value, type = "p", col = "blue", xlab = "c", ylab = "p-value", main = "p-value vs c")

plot(factor(results_df$c), p_value, type = "p", col = "blue", xlab = "c", ylab = "p-value", main = "p-value vs c", ylim=c(0,0.15))
