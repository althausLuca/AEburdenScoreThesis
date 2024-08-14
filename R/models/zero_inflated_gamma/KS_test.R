library(MASS)

trial_data <- load_shorter_trials()

trial <- trial_data$trials[[1]]

p_values <- trial_data$apply_to_each(function(trial) {

  fit <- fitdistr(trial$Score[trial$Score>0], "lognormal")$loglik
  fit$loglik
  fit$vcov
  params <- as.list(fit$estimate)
  params$x <- trial$Score[trial$Score>0]

  do.call(dlnorm, params)

  ks_result <- ks.test(trial$Score[trial$Score>0], "pgamma", shape = shape_est, rate = rate_est)
  return(ks_result$p.value)
})

p_values <- unlist(p_values)
sig_rate_shorter <- sum(p_values < 0.05) / length(p_values)