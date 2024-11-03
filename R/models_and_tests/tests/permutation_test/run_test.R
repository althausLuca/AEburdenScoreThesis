
run_test.permutation_test <- function(test, trial , n_permutations = test$parameters$n_permutations) {
  #result <- mean_permutation_test(trial, n_permutations = n_permutations , return_permutations = F)
  #p_value <- result$p_value

  # this yields the same p_value as the above commented code but is much faster
  control_i <- trial$Group == "control"
  treatment_i <- trial$Group == "treatment"

  samples <- replicate(n_permutations, sample(trial$Score, length(trial$Score), replace = FALSE))

  original_diff <- mean(trial$Score[treatment_i])-mean(trial$Score[control_i])
  perm_diff <- colMeans(samples[treatment_i,])-colMeans(samples[control_i,])
  p_value_fast <- 2*(min(sum( perm_diff >= original_diff)+1 , sum(perm_diff<=original_diff)+1)/(n_permutations+1))
  p_value <- p_value_fast

  return(create_test_result(test, p_value))
}


#' Permute trial data
#' @param trial A data frame containing the trial data (columns: Score, Group)
#' @return A data frame with the same data as the input, but with the Score column permuted
permute_trial <- function(trial) {
  trial$Score <- sample(trial$Score, length(trial$Score), replace = F) #null distribution
  trial$Group <- as.factor(trial$Group)
  trial$Group <- relevel(trial$Group, ref = "control")
  return(trial)
}

#' Get the means of the control and treatment groups
#' @param trial A data frame containing the trial data
#' @return A list containing the mean of the control and treatment groups
#' (control = mean of control group, treatment = mean of treatment group)
get_trial_means <- function(trial) {
  control_mean <- mean(trial$Score[trial$Group == "control"])
  treatment_mean <- mean(trial$Score[trial$Group == "treatment"])
  return(list(control = control_mean, treatment = treatment_mean))
}

#' Perform a permutation test to compare the means of the control and treatment groups
#' @param trial A data frame containing the trial data
#' @param n_permutations The number of permutations to perform (default = 10000)
#' @param n_sim alias for n_permutations
#' @param return_permutations A boolean indicating whether to return the used permutations of a trial
#' (default = FALSE, only return p-value and saves time and memory)
#'
#' @return A list containing the observed means, p-value and if return_permutations is TRUE, the permuted means
mean_permutation_test <- function(trial, n_permutations = 10000, return_permutations = F, n_sim = n_permutations) {
  if (return_permutations) {
    trial_permutation_means <- vector("list", n_sim) # Initialize list to store results
  }

  trial_means <- get_trial_means(trial)
  T_diff <- trial_means$treatment - trial_means$control
  T_abs_diff <- abs(T_diff)

  # keep track of the observed means
  T_p_is_greater <- 0
  T_p_is_lower <- 0
  T_p_abs_is_greater <- 0
  for (i in 1:n_sim) {
    permuted_trial <- permute_trial(trial)
    permuted_mean <- get_trial_means(permuted_trial)
    T_p_diff <- permuted_mean$treatment - permuted_mean$control
    permuted_abs_diff <- abs(permuted_mean$treatment - permuted_mean$control)
    if (T_diff <= T_p_diff) {
      T_p_is_greater <- T_p_is_greater + 1
    }
    if (T_diff >= T_p_diff) {
      T_p_is_lower <- T_p_is_lower + 1
    }
    if (T_abs_diff <= permuted_abs_diff) {
      T_p_abs_is_greater <- T_p_abs_is_greater + 1
    }
    if (return_permutations) {
      trial_permutation_means[[i]] <- permuted_mean
    }
  }

  p_value_abs <- (T_p_abs_is_greater + 1) / (n_sim + 1)
  p_value_l <- (T_p_is_lower + 1) / (n_sim + 1)
  p_value_g <- (T_p_is_greater + 1) / (n_sim + 1)
  p_value <- 2*min(p_value_g, p_value_l)


  if (return_permutations) {
    trial_permutation_means_df <- do.call(rbind, lapply(trial_permutation_means, as.data.frame))
    return(list(
      observed_means = trial_means,
      permuted_means = trial_permutation_means_df,
      p_value = p_value,
      p_value_abs = p_value_abs,
      p_value_l = p_value_l,
      p_value_g = p_value_g
    ))
  }

  return(list(
    observed_means = trial_means,
    p_value = p_value,
    p_value_abs = p_value_abs,
    p_value_l = p_value_l,
    p_value_g = p_value_g
  ))
}


