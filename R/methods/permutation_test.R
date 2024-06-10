
#' Permute trial data
permute_trial <- function(trial){
  trial$Score <- sample(trial$Score,length(trial$Score), replace=F) #null distribution
  trial$Group <- as.factor(trial$Group)
  trial$Group <- relevel(trial$Group, ref = "control")
  return(trial)
}

#' Get the means of the control and treatment groups
get_trial_means <- function(trial){
  control_mean <- mean(trial$Score[trial$Group=="control"])
  treatment_mean <- mean(trial$Score[trial$Group=="treatment"])
  return(list(control=control_mean, treatment=treatment_mean))
}

#' Perform a permutation test to compare the means of the control and treatment groups
#' @param trial A data frame containing the trial data
#' @param n_sim The number of simulations to perform
#' @param return_permutations A boolean indicating whether to return  all permutations (default = TRUE)
#'
mean_permutation_test <- function(trial, n_sim=10000 , return_permutations=T){
  if(return_permutations){
    trial_permutation_means <- vector("list", n_sim) # Initialize list to store results
  }

  trial_means <- get_trial_means(trial)
  abs_diff <- abs(trial_means$treatment - trial_means$control)

  # keep track of the observed means
  trial_diff_counter <- 0
  for (i in 1:n_sim) {
    permuted_trial <- permute_trial(trial) # Permute trial
    permuted_mean <- get_trial_means(permuted_trial)
    permuted_abs_diff <- abs(permuted_mean$treatment - permuted_mean$control)

    if (abs_diff <= permuted_abs_diff) {
      trial_diff_counter <- trial_diff_counter + 1 # Increment counter if observed difference is greater than permuted difference
    }
    if (return_permutations) {
      trial_permutation_means[[i]] <- permuted_mean
    }
  }

  p_value <- trial_diff_counter / n_sim # Calculate p-value

  if (return_permutations) {
    trial_permutation_means_df <- do.call(rbind, lapply(trial_permutation_means, as.data.frame))
    return(list(
      observed_means = trial_means,
      permuted_means = trial_permutation_means_df,
      p_value = p_value
    ))
  }

  return(list(
    observed_means = trial_means,
    p_value = p_value
  ))
}

