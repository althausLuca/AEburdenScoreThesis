mlogl <- function(alpha, x, Beta) {
  #- sum(dgamma(x, shape = alpha, scale=Beta, log = TRUE))
  # normal dist:
  -sum(dnorm(x, mean = alpha, sd = Beta, log = TRUE))
}

GetInput <- function(x){
  cont <- x[x>0]
  Npm <- length(x)-length(cont)
  Nc <- length(cont)
  p.hat <- Npm/length(x)
  muCont <- mean(cont, na.rm=TRUE)
  varCont <- var(cont, na.rm=TRUE)
  alpha <- varCont/(muCont^2)
  ans <- list(cont=numeric(), Npm=numeric(), Nc=numeric(),
              p.hat=numeric(), theta=numeric())
  ans$cont <- cont
  ans$Npm <- Npm
  ans$Nc <- Nc
  ans$p.hat <- p.hat
  ans$theta <- alpha
  return(ans)
}


# Parametric likelihood ratio test
LRT <- function(data, group, Beta) {
  Ho <- GetInput(data)
  Index1 <- c(group == 1)
  Group1 <- data[Index1]
  Group0 <- data[!Index1]
  Ha1 <- GetInput(Group1)
  Ha2 <- GetInput(Group0)
  if (Ho$Nc == 0) {
    flag <- 0
    null.logl <- 0
    G1.logl <- 0
    G0.logl <- 0
  }
  else if (Ha1$Nc == 0) {
    flag <- 0
    null.logl <- (Ho$Npm * log(Ho$p.hat)) + (Ho$Nc * log(1 - Ho$p.hat))
    if (Ha2$Npm == 0) {
      G1.logl <- (Ha1$Npm * log(Ha1$p.hat))
      G0.logl <- (Ha2$Nc * log(1 - Ha2$p.hat))
    } else {
      G1.logl <- (Ha1$Npm * log(Ha1$p.hat))
      G0.logl <- (Ha2$Npm * log(Ha2$p.hat)) + (Ha2$Nc * log(1 -
                                                              Ha2$p.hat))
    }
  }
  else if (Ha2$Nc == 0) {
    flag <- 0
    null.logl <- (Ho$Npm * log(Ho$p.hat)) + (Ho$Nc * log(1 - Ho$p.hat))
    if (Ha1$Npm == 0) {
      G1.logl <- (Ha1$Nc * log(1 - Ha1$p.hat))
      G0.logl <- (Ha2$Npm * log(Ha2$p.hat))
    } else {
      G1.logl <- (Ha1$Npm * log(Ha1$p.hat)) + (Ha1$Nc * log(1 -
                                                              Ha1$p.hat))
      G0.logl <- (Ha2$Npm * log(Ha2$p.hat))
    }
  }
  else if (Ha1$Nc == 1) {
    flag <- 0
    null.logl <- (Ho$Npm * log(Ho$p.hat)) + (Ho$Nc * log(1 - Ho$p.hat))
    if (Ha2$Npm == 0) {
      G1.logl <- (Ha1$Npm * log(Ha1$p.hat)) + (Ha1$Nc * log(1 -
                                                              Ha1$p.hat))
      G0.logl <- (Ha2$Nc * log(1 - Ha2$p.hat))
    } else {
      G1.logl <- (Ha1$Npm * log(Ha1$p.hat)) + (Ha1$Nc * log(1 -
                                                              Ha1$p.hat))
      G0.logl <- (Ha2$Npm * log(Ha2$p.hat)) + (Ha2$Nc * log(1 -
                                                              Ha2$p.hat))
    }
  }
  else if (Ha2$Nc == 1) {
    flag <- 0
    null.logl <- (Ho$Npm * log(Ho$p.hat)) + (Ho$Nc * log(1 - Ho$p.hat))
    if (Ha1$Npm == 0) {
      G1.logl <- (Ha1$Nc * log(1 - Ha1$p.hat))
      G0.logl <- (Ha2$Npm * log(Ha2$p.hat)) + (Ha2$Nc * log(1 -
                                                              Ha2$p.hat))
    } else {
      G1.logl <- (Ha1$Npm * log(Ha1$p.hat)) + (Ha1$Nc * log(1 -
                                                              Ha1$p.hat))
      G0.logl <- (Ha2$Npm * log(Ha2$p.hat)) + (Ha2$Nc * log(1 -
                                                              Ha2$p.hat))
    }
  }
  else {
    uniq1 <- length(unique(Group1[Group1 != 0]))
    uniq2 <- length(unique(Group0[Group0 != 0]))
    if ((uniq1 < 2) & (uniq2 < 2)) {
      flag <- 0
      if (Ho$Npm == 0) {
        null.logl <- 0
        G1.logl <- 0
        G0.logl <- 0
      } else {
        null.logl <- (Ho$Npm * log(Ho$p.hat)) + (Ho$Nc * log(1 -
                                                               Ho$p.hat))
        G1.logl <- (Ha1$Npm * log(Ha1$p.hat)) + (Ha1$Nc * log(1 -
                                                                Ha1$p.hat))
        G0.logl <- (Ha2$Npm * log(Ha2$p.hat)) + (Ha2$Nc * log(1 -
                                                                Ha2$p.hat))
      }
    }
    else if (Ho$Npm == 0) {
      flag <- 0
      out <- nlminb(Ho$theta, mlogl, x = Ho$cont, Beta = Beta,
                    lower = 0.0000001, upper = Inf)
      out.1 <- nlminb(Ha1$theta, mlogl, x = Ha1$cont,
                      Beta = Beta, lower = 0.0000001, upper = Inf)
      out.0 <- nlminb(Ha2$theta, mlogl, x = Ha2$cont,
                      Beta = Beta, lower = 0.0000001, upper = Inf)
      null.logl <- sum(dgamma(Ho$cont, shape = out$par / Beta,
                              scale = Beta, log = TRUE))
      G1.logl <- sum(dgamma(Ha1$cont, shape = out.1$par / Beta,
                            scale = Beta, log = TRUE))
      G0.logl <- sum(dgamma(Ha2$cont, shape = out.0$par / Beta,
                            scale = Beta, log = TRUE))
    }
    else if (Ha1$Npm == 0) {
      flag <- 1
      out <- nlminb(Ho$theta, mlogl, x = Ho$cont, Beta = Beta,
                    lower = 0.0000001, upper = Inf)
      out.1 <- nlminb(Ha1$theta, mlogl, x = Ha1$cont,
                      Beta = Beta, lower = 0.0000001, upper = Inf)
      out.0 <- nlminb(Ha2$theta, mlogl, x = Ha2$cont,
                      Beta = Beta, lower = 0.0000001, upper = Inf)
      null.logl <- (Ho$Npm * log(Ho$p.hat)) +
        (Ho$Nc * log(1 -
                       Ho$p.hat)) +
        sum(dgamma(Ho$cont, shape =
          out$par / Beta, scale = Beta, log = TRUE))
      G1.logl <- sum(dgamma(Ha1$cont, shape = out.1$par / Beta,
                            scale = Beta, log = TRUE))
      G0.logl <- (Ha2$Npm * log(Ha2$p.hat)) +
        (Ha2$Nc * log(1 -
                        Ha2$p.hat)) +
        sum(dgamma(Ha2$cont, shape =
          out.0$par / Beta, scale = Beta, log = TRUE))
    }
    else if (Ha2$Npm == 0) {
      flag <- 1
      out <- nlminb(Ho$theta, mlogl, x = Ho$cont, Beta = Beta,
                    lower = 0.0000001, upper = Inf)
      out.1 <- nlminb(Ha1$theta, mlogl, x = Ha1$cont,
                      Beta = Beta, lower = 0.0000001, upper = Inf)
      out.0 <- nlminb(Ha2$theta, mlogl, x = Ha2$cont,
                      Beta = Beta, lower = 0.0000001, upper = Inf)
      null.logl <- (Ho$Npm * log(Ho$p.hat)) +
        (Ho$Nc * log(1 -
                       Ho$p.hat)) +
        sum(dgamma(Ho$cont, shape =
          out$par / Beta, scale = Beta, log = TRUE))
      G1.logl <- (Ha1$Npm * log(Ha1$p.hat)) +
        (Ha1$Nc * log(1 -
                        Ha1$p.hat)) +
        sum(dgamma(Ha1$cont, shape =
          out.1$par / Beta, scale = Beta, log = TRUE))
      G0.logl <- sum(dgamma(Ha2$cont, shape = out.0$par / Beta,
                            scale = Beta, log = TRUE))
    }
    else {
      flag <- 1
      out <- nlminb(Ho$theta, mlogl, x = Ho$cont, Beta = Beta,
                    lower = 0.0000001, upper = Inf)
      out.1 <- nlminb(Ha1$theta, mlogl, x = Ha1$cont,
                      Beta = Beta, lower = 0.0000001, upper = Inf)
      out.0 <- nlminb(Ha2$theta, mlogl, x = Ha2$cont,
                      Beta = Beta, lower = 0.0000001, upper = Inf)
      null.logl <- (Ho$Npm * log(Ho$p.hat)) +
        (Ho$Nc * log(1 -
                       Ho$p.hat)) +
        sum(dgamma(Ho$cont, shape =
          out$par / Beta, scale = Beta, log = TRUE))
      G1.logl <- (Ha1$Npm * log(Ha1$p.hat)) +
        (Ha1$Nc * log(1 -
                        Ha1$p.hat)) +
        sum(dgamma(Ha1$cont, shape =
          out.1$par / Beta, scale = Beta, log = TRUE))
      G0.logl <- (Ha2$Npm * log(Ha2$p.hat)) +
        (Ha2$Nc * log(1 -
                        Ha2$p.hat)) +
        sum(dgamma(Ha2$cont, shape =
          out.0$par / Beta, scale = Beta, log = TRUE))
    }
  }
  logl <- G1.logl + G0.logl
  X2 <- -2 * (null.logl - logl)
  if (flag == 0) {
    pv <- 1 - pchisq(X2, 1)
  } else
    pv <- 1 - pchisq(X2, 2)
  ans <- list(statistic = X2, pvalue = pv)
  return(ans)
}



test_ <- function(trial) LRT(trial$Score, (trial$Group!="control")*1, 100)

source("../../../trials/trial_loader.R")

trial_data <- load_equal_trials()
results <- trial_data$apply_to_each(test_, as.df = TRUE)
p_values <- unlist(results$pvalue)

sig_rate_equal <- sum(p_values < 0.05) / length(p_values)
sig_rate_equal

library(MASS)

first_trial <- trial_data$trials[[1]]
Scores <- first_trial$Score

fitdistr(Scores[Scores>0], densfun="gamma")
fitdistr(Scores[Scores>0], densfun="gamma" , rate=10)
fitdistr(Scores[Scores>0], densfun="normal")
fitdistr(Scores[Scores>0], densfun="normal", sd=10)

fitdistr(Scores[Scores>0], densfun="lognormal" ,meanlog = 10)$loglik
fitdistr(Scores[Scores>0], densfun="lognormal")$loglik

fitdistr(Scores[Scores>0], densfun="gamma", rate= 0.06793645 )$loglik

library("fitdistrplus")
fitdistr(Scores[Scores>0], densfun="gamma")$loglik
fitdist(Scores[Scores>0], "gamma")$loglik

fit_dist <- fitdist(Scores[Scores>0], "lnorm" )
ll <- fit_dist$loglik
estimates <- fit_dist$estimate

sdlog <- unname(fit_dist$estimate[2])
fitdist(Scores[Scores>0], "lnorm" , fix.arg = list(sdlog = sdlog))$loglik