#'try out different methods for quantile regression

library(quantreg)
library(ggplot2)

source("R/trials/trial_simulation.R")


?rq

#Scenario_3_k_1.5_l_3.5.csv
#Scenario_2_k_1.5_s_0.5.csv
#Scenario_1_k_1.5.csv
trial_file <- "Scenario_2_k_1.5_s_0.5.csv"

trial_data <- get_trial_data(trial_file)

q <- 0.95
first_trial <- trial_data$trials[[1]]
#plot(factor(first_trial$Group), first_trial$Score)
model <- rq(Score ~ Group, data = first_trial, tau = q)

ker_p_values <- summary(model, se = "ker")$coef[2, 4]
boot_p_values <- summary(model, se = "boot")$coef[2, 4]

iid_p_values <- summary(model, se = "iid")$coef[2, 4]
nid_p_values <- summary(model, se = "nid")$coef[2, 4]

#rank_p_values <- summary(model, se="rank")$coef[,4] # not working only gives bounds
#extreme_p_values <- summary(model, se="extreme")$coef[,4]

for (i in seq_along(trial_data$trials)) {
  if(i == 1) { next }
  if (i > 1000) { break }

  trial <- trial_data$trials[[i]]

  model_result <- rq(Score ~ Group, data = trial, tau = q, method = "br")
  print("ker")
  ker_p_values <- c(ker_p_values, summary(model_result, se = "ker")$coef[2, 4])
  print("boot")
  boot_p_values <- c(boot_p_values, summary(model_result, se = "boot")$coef[2, 4])
  #iid_p_values <- c(iid_p_values, summary(model_result, se = "iid")$coef[2, 4])
  print("nid")
  nid_p_values <- tryCatch(
    { c(nid_p_values, summary(model_result, se = "nid")$coef[2, 4])} ,
     error = function(x) return(c(nid_p_values,2))
  )
}

plot(sort(ker_p_values), type = "l", col = "blue", xlab = "Trial", ylab = "p-value", main = "p-value vs Trial")
lines(sort(boot_p_values), col = "red")
#lines(sort(iid_p_values), col = "green")
lines(sort(nid_p_values), col = "purple")
abline

legend("topright", legend = c("ker", "boot", "iid", "nid"), col = c("blue", "red", "green", "purple"), lty = 1:1, cex = 0.8)

