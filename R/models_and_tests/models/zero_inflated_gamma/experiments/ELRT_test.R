GetInput <- function(x) {
  cont <- x[x > 0]
  Npm <- length(x) - length(cont)
  Nc <- length(cont)
  p.hat <- Npm / length(x)
  muCont <- mean(cont, na.rm = TRUE)
  varCont <- var(cont, na.rm = TRUE)
  alpha <- varCont / (muCont^2)
  ans <- list(cont = numeric(), Npm = numeric(), Nc = numeric(),
              p.hat = numeric(), theta = numeric())
  ans$cont <- cont
  ans$Npm <- Npm
  ans$Nc <- Nc
  ans$p.hat <- p.hat
  ans$theta <- alpha
  return(ans)
}


GetWeights <- function(fit, contX, contY) {
  Lambda <- fit$estimate[1]
  Mean <- fit$estimate[2]
  Nx <- length(contX)
  Ny <- length(contY)
  Nt <- Nx + Ny
  fx <- Nt / Nx
  fy <- Nt / Ny
  Zx <- 1 - fx * Lambda * (contX - Mean)
  Zy <- 1 + fy * Lambda * (contY - Mean)
  wts <- cbind(sum(c(1 / (Nx * Zx))), sum(c(1 / (Ny * Zy))))
  return(wts)
}

ELRT <- function(data, group) {
  Ho <- GetInput(data)
  Index1 <- c(group == 1)
  Group1 <- data[Index1]
  Group0 <- data[!Index1]
  Ha1 <- GetInput(Group1)
  Ha2 <- GetInput(Group0)
  BigMean <- mean(Ho$cont, na.rm = TRUE)
  if (Ho$Nc == 0) {
    flag <- 0
    null.logl <- 0
    G1.logl <- 0
    G0.logl <- 0
    contELRT <- 0
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
    contELRT <- 0
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
    contELRT <- 0
  }
  else if (Ha1$Nc == 1) {
    flag <- 0
    null.logl <- (Ho$Npm * log(Ho$p.hat)) + (Ho$Nc * log(1 - Ho$p.hat))
    contELRT <- 0
    if (Ha2$Npm == 0) {
      G1.logl <- (Ha1$Npm * log(Ha1$p.hat)) + (Ha1$Nc * log(1 -
                                                              Ha1$p.hat))
      G0.logl <- (Ha2$Nc * log(1 - Ha2$p.hat))
      contELRT <- 0
    } else {
      G1.logl <- (Ha1$Npm * log(Ha1$p.hat)) + (Ha1$Nc * log(1 -
                                                              Ha1$p.hat))
      G0.logl <- (Ha2$Npm * log(Ha2$p.hat)) + (Ha2$Nc * log(1 -
                                                              Ha2$p.hat))
      contELRT <- 0
    }
  } else if (Ha2$Nc == 1) {
    flag <- 0
    null.logl <- (Ho$Npm * log(Ho$p.hat)) + (Ho$Nc * log(1 - Ho$p.hat))
    if (Ha1$Npm == 0) {
      G1.logl <- (Ha1$Nc * log(1 - Ha1$p.hat))
      G0.logl <- (Ha2$Npm * log(Ha2$p.hat)) + (Ha2$Nc * log(1 -
                                                              Ha2$p.hat))
      contELRT <- 0
    } else {
      G1.logl <- (Ha1$Npm * log(Ha1$p.hat)) + (Ha1$Nc * log(1 -
                                                              Ha1$p.hat))
      G0.logl <- (Ha2$Npm * log(Ha2$p.hat)) + (Ha2$Nc * log(1 -
                                                              Ha2$p.hat))
      contELRT <- 0
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
        contELRT <- 0
      }
      else {
        null.logl <- (Ho$Npm * log(Ho$p.hat)) + (Ho$Nc * log(1 -
                                                               Ho$p.hat))
        G1.logl <- (Ha1$Npm * log(Ha1$p.hat)) + (Ha1$Nc * log(1 -
                                                                Ha1$p.hat))
        G0.logl <- (Ha2$Npm * log(Ha2$p.hat)) + (Ha2$Nc * log(1 -
                                                                Ha2$p.hat))
        contELRT <- 0
      }
    }
    else if (Ho$Npm == 0) {
      flag <- 0
      fit <- nlm(MeanELRT, p = c(0, BigMean), x = Ha1$cont,
                 y = Ha2$cont, hessian = FALSE, gradtol = 1e-15)
      Lambda <- fit$estimate[1]
      Mean <- fit$estimate[2]
      SumWts <- GetWeights(fit, contX = Ha1$cont,
                           contY = Ha2$cont)
      if (any(SumWts < 0.95)) contELRT <- NA
      else contELRT <- fit$minimum
      null.logl <- 0
      G1.logl <- 0
      G0.logl <- 0
    }
    else if (Ha1$Npm == 0) {
      flag <- 1
      fit <- nlm(MeanELRT, p = c(0, BigMean), x = Ha1$cont,
                 y = Ha2$cont, hessian = FALSE, gradtol = 1e-9)
      Lambda <- fit$estimate[1]
      Mean <- fit$estimate[2]
      SumWts <- GetWeights(fit, contX = Ha1$cont, contY = Ha2$cont)
      if (any(SumWts < 0.95)) contELRT <- NA
      else contELRT <- fit$minimum
      null.logl <- (Ho$Npm * log(Ho$p.hat)) + (Ho$Nc * log(1 -
                                                             Ho$p.hat))
      G1.logl <- 0
      G0.logl <- (Ha2$Npm * log(Ha2$p.hat)) + (Ha2$Nc * log(1 -
                                                              Ha2$p.hat))
    }
    else if (Ha2$Npm == 0) {
      flag <- 1
      fit <- nlm(MeanELRT, p = c(0, BigMean), x = Ha1$cont,
                 y = Ha2$cont, hessian = FALSE, gradtol = 1e-9)
      Lambda <- fit$estimate[1]
      Mean <- fit$estimate[2]
      SumWts <- GetWeights(fit, contX = Ha1$cont,
                           contY = Ha2$cont)
      if (any(SumWts < 0.95)) contELRT <- NA
      else contELRT <- fit$minimum
      null.logl <- (Ho$Npm * log(Ho$p.hat)) + (Ho$Nc * log(1 -
                                                             Ho$p.hat))
      G1.logl <- (Ha1$Npm * log(Ha1$p.hat)) + (Ha1$Nc * log(1 -
                                                              Ha1$p.hat))
      G0.logl <- 0
    }
    else {
      flag <- 1
      fit <- nlm(MeanELRT, p = c(0, BigMean,BigMean*2), x = Ha1$cont,
                 y = Ha2$cont, hessian = FALSE, gradtol = 1e-15,iterlim = 300)
      Lambda <- fit$estimate[1]
      Mean <- fit$estimate[2]
      SumWts <- GetWeights(fit, contX = Ha1$cont, contY = Ha2$cont)
      if (any(SumWts < 0.95)) {
        contELRT <- NA
        print(min(SumWts))
        print(fit$code)
        print(fit$estimate)
      }
      else {
        contELRT <- fit$minimum
      }

      null.logl <- Ho$Npm * log(Ho$p.hat) + Ho$Nc * log(1 - Ho$p.hat)
      G1.logl <- Ha1$Npm * log(Ha1$p.hat) + Ha1$Nc * log(1 - Ha1$p.hat)
      G0.logl <- Ha2$Npm * log(Ha2$p.hat) + Ha2$Nc * log(1 - Ha2$p.hat)
    }
  }


  logl <- G1.logl + G0.logl
  X2 <- -2 * (null.logl - logl + contELRT)
  if (flag == 0) {
    pv <- 1 - pchisq(X2, 1)
    q <- 1
  } else {
    pv <- 1 - pchisq(X2, 2)
    q <- 2
  }
  N <- length(data)
  Fpv <- 1 - pf(X2 * (N - q) / ((N - 1) * q), q, N - q)
  ans <- list(statistic = X2, X2pvalue = pv, Fpvalue = Fpv)
  return(ans)
}

log.star <- function(z, eps) {
  ans <- z
  lo <- (z < eps)
  ans[lo] <- log(eps) - 1.5 + 2 * z[lo] / eps - 0.5 *
    (z[lo] / eps)^2
  ans[!lo] <- log(z[!lo])
  ans
}


# Two sample ELRT for test of differences in means
MeanELRT <- function(params, x, y) {
  Nx <- length(x)
  Ny <- length(y)
  Nt <- Nx + Ny
  fx <- Nt / Nx
  fy <- Nt / Ny
  Zx <- 1 - fx * params[1] * (x - params[2])
  Zy <- 1 + fy * params[1] * (y - params[2])
  return(-sum(log.star(Zy, 1 / Nx)) - sum(log.star(Zx, 1 / Ny)))
}


source("../../../trials/trial_loader.R")


test_ <- function(trial) {
  x <- trial$Score
  group <- trial$Group

  # map group to 1 and 0
  group <- as.numeric(group == "treatment")
  return(ELRT(x, group))
}


p_value_type <- "Fpvalue" # "X2pvalue" # "Fpvalue"

trial_data <- load_equal_trials()
df <- trial_data$apply_to_each(test_, as.df = TRUE)

na_count_equal <- sum(is.na(df[[p_value_type]]))
p_values <- df[[p_value_type]][!is.na(df[[p_value_type]])]
p_value_rate_equal <- sum(p_values < 0.05) / length(p_values)

trial_data <- load_longer_trials()
df <- trial_data$apply_to_each(test_, as.df = TRUE)

na_count_longer <- sum(is.na(df[[p_value_type]]))
p_values <- df[[p_value_type]][!is.na(df[[p_value_type]])]
p_value_rate_longer <- sum(p_values < 0.05) / length(p_values)


trial_data <- load_shorter_trials()
df <- trial_data$apply_to_each(test_, as.df = TRUE)

na_count_shorter <- sum(is.na(df[[p_value_type]]))
p_values <- df[[p_value_type]][!is.na(df[[p_value_type]])]
p_value_rate_shorter <- sum(p_values < 0.05) / length(p_values)


print(paste0("used p_value_type:", p_value_type))
print(paste0("Type 1 Error:", p_value_rate_equal))
print(paste0("Power (Longer event durations):", p_value_rate_longer))
print(paste0("Power (Shorter event gap times):", p_value_rate_shorter))
print(paste0("NA count equal:", na_count_equal))
print(paste0("NA count longer:", na_count_longer))
print(paste0("NA count shorter:", na_count_shorter))


na_indices <- seq(df$X2pvalue)[is.na(df$X2pvalue)]
non_na_indices <- seq(df$X2pvalue)[!is.na(df$X2pvalue)]
na_indices[1]
na_trial <- trial_data$trials[[na_indices[3]]]

na_trials_max <- sapply(na_indices, function(index) var(trial_data$trials[[index]]$Score))
non_na_trials_max <- sapply(non_na_indices, function(index) var(trial_data$trials[[index]]$Score))


mean(na_trials_max)
mean(non_na_trials_max)

max(na_trial$Score)
max(non_na_trial$Score)
