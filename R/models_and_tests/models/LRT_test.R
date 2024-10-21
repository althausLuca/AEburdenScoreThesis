library(statmod)
library(MASS)
library(fitdistrplus)


log_likelihood <- function(Scores, dist = "gamma", fix.arg = NULL) {
  p <- mean(Scores == 0)
  print(dist)

  if (p == 0) {
    p <- 0.0001
  }

  if (length(Scores[Scores > 0]) < 2) {
    return(list(ll = log(p) * sum(Scores == 0), estimates = rep(NA, 2)))
  }

  start <- NULL

  fit_dist <- tryCatch({
    fitdist(Scores[Scores > 0], dist, calcvcov = FALSE, keepdata = FALSE, fix.arg = fix.arg)
  }, error = function(e) { # 1 of 5000 frials failed due to not providing starting values

    if (dist == "gamma") {
      start <- list(shape = 1, rate = 0.1)
    }
    if (dist == "lnorm") {
      start <- list(meanlog = 1, sdlog = 1)
    }

    # remove fixed arg from starting values
    if (!is.null(fix.arg)) {
      start <- start[!names(start) %in% names(fix.arg)]
    }
    fitdist(Scores[Scores > 0], dist, calcvcov = FALSE, keepdata = FALSE, start = start)
  })

  ll_non_zero <- fit_dist$loglik + sum(Scores > 0) * log(1 - p)

  if (p == 0) {
    ll_zero <- -Inf
  }
  else {
    ll_zero <- log(p) * sum(Scores == 0)
  }

  return(list(ll = ll_zero + ll_non_zero, estimates = fit_dist$estimate))
}

LRT_test <- function(trial, dist = "gamma", fix_arg = TRUE , full_results=FALSE) {

  if (!(dist %in% c("gamma", "lnorm", "norm"))) {
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

  if(full_results){
    return(list(p_value = p_value,
                LRT_statistic = LRT_statistic,
                ll_H0 = ll_H0,
                ll_HA = ll_HA))
  }
  return(p_value)
}




