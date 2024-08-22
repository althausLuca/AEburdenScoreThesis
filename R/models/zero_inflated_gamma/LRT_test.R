library(statmod)
library(MASS)
library(fitdistrplus)


log_likelihood <- function(Scores, dist = "gamma", ...) {
  p <- mean(Scores == 0)
  print(dist)
  fit_dist <- fitdist(Scores[Scores > 0], dist, calcvcov = FALSE, keepdata = FALSE, ...)
  ll_non_zero <- fit_dist$loglik + sum(Scores > 0) * log(1 - p)
  ll_zero <- log(p) * sum(Scores == 0)

  return(list(ll = ll_zero + ll_non_zero, estimates = fit_dist$estimate))
}

LRT_test <- function(trial, dist = "gamma", fix_arg = TRUE) {
  if(!(dist %in% c("gamma", "lnorm", "norm"))) {
    stop("Distribution not supported")
  }
  r_HO <- log_likelihood(trial$Score, dist = dist)
  ll_H0 <- r_HO$ll

  if (fix_arg) {
    fix.arg <- switch(dist,
                      "gamma" = list(rate = unname(r_HO$estimates[2])),
                      "norm" = list(sd = unname(r_HO$estimates[2])),
                      "lnorm" = list(sdlog = unname(r_HO$estimates[2]))
    )
    df <- 2
  }
  else {
    fix.arg <- NULL
    df <- 3
  }

  ll_HA <- log_likelihood(trial$Score[trial$Group == "treatment"], dist = dist, fix.arg = fix.arg)$ll +
    log_likelihood(trial$Score[trial$Group == "control"], dist = dist, fix.arg = fix.arg)$ll

  LRT_statistic <- -2 * (ll_H0 - ll_HA)
  p_value <- pchisq(LRT_statistic, df = df, lower.tail = FALSE)
  return(p_value)
}


source("R/trials/trial_loader.R")

#
# # test_ <-function(trial) t.test(trial$Score[trial$Group=="treatment"] , trial$Score[trial$Group=="control"])$p.value
# dist <- "norm" # "gamma" , "normal"
#
# test_ <- function(trial) LRT_test(trial, dist = dist , fix_arg = FALSE)
#
#
# trial_data <- load_equal_trials()
# p_values <- trial_data$apply_to_each(test_)
# p_values <- unlist(p_values)
# sig_rate_equal <- sum(p_values < 0.05) / length(p_values)
#
#
# trial_data <- load_longer_trials()
# p_values <- trial_data$apply_to_each(test_)
# p_values <- unlist(p_values)
# sig_rate_longer <- sum(p_values < 0.05) / length(p_values)
#
#
# trial_data <- load_shorter_trials()
# p_values <- trial_data$apply_to_each(test_)
# p_values <- unlist(p_values)
# sig_rate_shorter <- sum(p_values < 0.05) / length(p_values)
#
#
# print(paste0("Continious Distribution used:", dist))
# print(paste0("Type 1 Error:", sig_rate_equal))
# print(paste0("Power (Longer event durations):", sig_rate_longer))
# print(paste0("Power (Shorter event gap times):", sig_rate_shorter))


