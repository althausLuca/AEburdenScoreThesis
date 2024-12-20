#' @param trial A data frame with columns Score and Group
#' @param test A string specifying the test to use for the T statistic. Options are "ttest" (default), "welch", "ks", and "wilcoxon"
#' @return A list with the B statistic, T statistic, and p-value
#' @description This function calculates the B statistic, T statistic, and p-value for a two part test
#' @referenced in the paper Hypothesis Tests for Point-Mass Mixture Data with Application to `Omics Data with Many Zero Values
#' by Sandra Taylor and Katherine Pollard
two_part_test <- function(trial, test = "ttest") {
  B <- get_B(trial)
  T <- get_T(trial, test = test)

  V_squared <- B^2 + T^2

  df <- 2 - (isFALSE(B) || isFALSE(T))

  p_value <- unname(1 - pchisq(V_squared, df = df))
  return(list(B = B, T = T, p_value = p_value))
}

get_B <- function(trial) {
  treatment_scores <- trial$Score[trial$Group == "treatment"]
  control_scores <- trial$Score[trial$Group == "control"]

  n_treatment <- length(treatment_scores)
  n_control <- length(control_scores)

  n_treatment_0 <- sum(treatment_scores == 0)
  n_control_0 <- sum(control_scores == 0)

  if (n_treatment_0 + n_control_0 == 0) {
    return(FALSE)
  }

  p_hat_control <- (n_control_0 - 0.5) / n_control
  p_hat_treatment <- (n_treatment_0 + 0.5) / n_treatment

  p_hat <- (n_control_0 + n_treatment_0) / (n_control + n_treatment)

  p_mult_term <- p_hat * (1 - p_hat)
  B <- (p_hat_control - p_hat_treatment) / (p_mult_term / n_treatment + p_mult_term / n_control)^0.5

  return(B)
}

get_T <- function(trial, test = "ttest") {
  treatment_scores <- trial$Score[trial$Group == "treatment"]
  treatment_scores.c <- treatment_scores[treatment_scores > 0]
  control_scores <- trial$Score[trial$Group == "control"]
  control_scores.c <- control_scores[control_scores > 0]

  n_treatment_c <- length(treatment_scores.c)
  n_control_c <- length(control_scores.c)

  if (n_treatment_c < 2 || n_control_c < 2) {
    return(FALSE) # if there are less than 2 non-zero values in either group, return 0 and only use B statistic
  }

  if (test == "welch") {
    T <- t.test(control_scores.c, treatment_scores.c, alternative = "two.sided", var.equal = FALSE)$statistic
  }else if (test == "ttest") {
    T <- t.test(control_scores.c, treatment_scores.c, alternative = "two.sided", var.equal = TRUE)$statistic
  }else if (test == "ks") {
    T <- ks.test(control_scores.c, treatment_scores.c, alternative = "two.sided")$statistic
  }else if (test == "wilcoxon") {
    m_c <- n_control_c # first group
    n_c <- n_treatment_c

    R <- sum(rank(c(control_scores.c, treatment_scores.c))[1:m_c]) # rank sum of the first group
    U <- m_c * n_c + m_c * (m_c + 1) / 2 - R

    mu <- m_c * n_c / 2
    sigma <- sqrt(m_c * n_c * (m_c + n_c + 1) / 12)

    contininutiy_correction <- 0.5
    if(U - mu < 0) {
      contininutiy_correction <- -0.5
    }
    T <- (U - mu - contininutiy_correction) / sigma

    # T.2 <- (abs(U - mu) - 0.5) / sigma
    # if(abs(T) != (T.2)) {
    #   print("T and T.2 are different")
    #   print(T)
    #   print(T.2)
    # }
  } else {
    stop("Invalid test type")
  }

  return(unname(T))
}

