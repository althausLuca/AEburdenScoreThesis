library(statmod)
library(MASS)


log_likelihood <- function(Scores , dist = "gamma") {
  p <- mean(Scores == 0)
  ll_non_zero <- fitdistr(Scores[Scores>0], dist)$loglik + sum(Scores > 0)*log(1 - p)
  ll_zero <- log(p) * sum(Scores == 0)

  return(ll_zero + ll_non_zero)
}

LRT_test <- function(trial , dist = "gamma") {
  ll_H0 <- log_likelihood(trial$Score , dist = dist)
  ll_HA <- log_likelihood(trial$Score[trial$Group == "treatment"], dist = dist) + log_likelihood(trial$Score[trial$Group == "control"] , dist = dist)
  LRT_statistic <- -2 * (ll_H0 - ll_HA)
  p_value <- pchisq(LRT_statistic, df = 2, lower.tail = FALSE)
  return(p_value)
}


source("R/trials/trial_loader.R")


# test_ <-function(trial) t.test(trial$Score[trial$Group=="treatment"] , trial$Score[trial$Group=="control"])$p.value
dist <- "lognormal" # "gamma" , "normal"

test_ <- function(trial) LRT_test(trial, dist = dist)


trial_data <- load_equal_trials()
p_values <- trial_data$apply_to_each(test_)
p_values <- unlist(p_values)
sig_rate_equal <- sum(p_values < 0.05) / length(p_values)


trial_data <- load_longer_trials()
p_values <- trial_data$apply_to_each(test_)
p_values <- unlist(p_values)
sig_rate_longer <- sum(p_values < 0.05) / length(p_values)


trial_data <- load_shorter_trials()
p_values <- trial_data$apply_to_each(test_)
p_values <- unlist(p_values)
sig_rate_shorter <- sum(p_values < 0.05) / length(p_values)


print(paste0("Continious Distribution used:", dist))
print(paste0("Type 1 Error:", sig_rate_equal))
print(paste0("Power (Longer event durations):", sig_rate_longer))
print(paste0("Power (Shorter event gap times):", sig_rate_shorter))


