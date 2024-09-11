source("R/trials/trial_loader.R")
source("R/models_and_tests/models_and_tests.R")


trial_data <- load_longer_trials()

test <- WILCOXON_TEST()

trial <- trial_data$trials[[9]]

non.zero_trial <- trial[trial$Score > 0,]

wilcoxon_test <- function(trial) {
  control_scores <- trial$Score[trial$Group == "control"]
  treatment_scores <- trial$Score[trial$Group == "treatment"]

  n_control <- length(control_scores)
  n_treatment <- length(treatment_scores)

  m_c <- n_control # first group
  n_c <- n_treatment

  R <- sum(rank(c(control_scores, treatment_scores))[1:m_c]) # rank sum of the first group
  R2 <- sum(rank(c(treatment_scores, control_scores))[1:n_treatment]) # rank sum of the first group
  U <- m_c * n_c + m_c * (m_c + 1) / 2 - R

  mu <- m_c * n_c / 2
  sigma <- sqrt(m_c * n_c * (m_c + n_c + 1) / 12)

  T <- (abs(U - mu) - 0.5) / sigma
  p_value <- 2 * pnorm(-abs(T))
  return(list(T = T, R = R, U = U, R2 = R2, p_value = p_value))
}


T <- wilcoxon_test(trial)

control_scores <- trial$Score[trial$Group == "control"]
treatment_scores <- trial$Score[trial$Group == "treatment"]

wilcox.test(control_scores, treatment_scores, exact = FALSE, correct = TRUE)$p.value

wilcox.test(control_scores, treatment_scores, exact = FALSE, correct = TRUE)$statistic

#
# combined_scores <- c(control_scores, treatment_scores)
# n_c <- length(control_scores)
# n_t <- length(treatment_scores)
#
# ranks <- rank(combined_scores,ties.method = "average")
#
# rank_sum <- sum(ranks[1:length(control_scores)])
#
# #subtract m(m+1)/2 from the ranks of the second group
# rank_sum_sub <- rank_sum - 100*(100+1)/2
#
# tie_counts <- t <- length(ranks) -  length(unique(ranks))
# tie_correction <- t*(t^2 - 1)/length(ranks)
#
# mean <- n_c*(n_t + n_c + 1)/2
# sigma  <- (n_t*n_c*(n_c + n_t + 1 - tie_correction )/12 )^0.5
#
# T <- (abs(rank_sum - mean) -0.5)/sigma
#
# p_value <- 2*pnorm(-abs(T))
# p_value
#

tied_adj_test <- function(trial) {
  control_scores <- trial$Score[trial$Group == "control"]
  treatment_scores <- trial$Score[trial$Group == "treatment"]

  combined_scores <- c(control_scores, treatment_scores)
  n_c <- length(control_scores)
  n_t <- length(treatment_scores)
  n <- n_c + n_t

  # Rank the combined scores
  ranks <- rank(combined_scores, ties.method = "average")

  # Calculate the tie adjustment
  tie_counts <- table(ranks) # Count occurrences of each rank
  tie_adjustment <- sum((tie_counts^3 - tie_counts)) / (n * (n - 1))

  # Calculate rank sums for both groups
  rank_sum.1 <- sum(ranks[1:n_c])
  rank_sum.2 <- sum(ranks) - rank_sum.1

  # Use the smaller rank sum for the test statistic
  rank_sum <- rank_sum.1 #min(rank_sum.1, rank_sum.2)

  # Calculate mean and standard deviation under the null hypothesis
  mu_W <- n_c * (n + 1) / 2
  sigma_W <- sqrt(n_c * n_t * ((n + 1) - tie_adjustment) / 12)

  # Z-statistic (without continuity correction for now)
  z <- (abs(rank_sum - mu_W) - 0.5) / sigma_W

  # Two-tailed p-value from Z-statistic
  p_value <- 2 * pnorm(-abs(z))
  return(list(p_value = p_value, "tie_correction" = tie_adjustment, mu_W = mu_W, sigma_W = sigma_W))
}


add_zero_to_each_group <- function(trial, n = 1) {
  trial <- rbind(trial, data.frame(Group = rep(c("control", "treatment"), n), Score = rep(c(0, 0), n)))
  return(trial)
}

trial <- trial_data$trials[[5]]
trial <- trial[trial$Score > 0,]

zero_to_add <- c(1, 10, 20, 30, 50, 100, 1000)
p_values <- c()
p_values_w <- c()
for (i in c(1, 10, 20, 30, 50, 60,80, 99,100, 1000 , 10000)) {
  trial_ <- add_zero_to_each_group(trial, i)
  result <- tied_adj_test(trial_)
  print(unname(unlist(result)))

  control_scores <- trial_$Score[trial_$Group == "control"]
  treatment_scores <- trial_$Score[trial_$Group == "treatment"]

  p_value <- wilcox.test(control_scores, treatment_scores, exact = FALSE, correct = TRUE)$p.value
  p_values_w <- c(p_values_w, p_value)
  p_values <- c(p_values, result$p_value)
}

rank(p_values)
p_values - p_values_w

