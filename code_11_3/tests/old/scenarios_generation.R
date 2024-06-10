source("../../R/Scenarios.R")
source("../../R/methods/run_methods.R")
# install and load the tidyverse package
library(tidyverse)

# create some data frames
df1 <- data.frame(x = 1:3, y = 4:6, u = 1:3)
df2 <- data.frame(a = letters[1:3], b = LETTERS[1:3])
df3 <- data.frame(m = 7:9, n = 10:12)

score_list <- list()
for (i in 1:3) {
  score_df <- data.frame(v = 1:i, group = rep("group", i))
  score_list <- c(score_list, list(score_df))
}
score_list[3]
# create a tibble that contains other data frames
df <- tibble(df1 = df1, df2 = df2, scores = score_list)
df$df1$x
df$scores
df[1,]$df2
df[1]
df[, 1]

## test scenario generation
n_sim <- 100
scenario <- TESTSCENARIO
score_list <- list()
print("gemerating scores")
for (index in 1:n_sim) {
  simulated_AEs.control <- simulate_events(scenario$control_AEs, max_time = 180, n_it = scenario$observations)
  simulated_AEs.treatment <- simulate_events(scenario$treatment_AEs, max_time = 180, n_it = scenario$observations)
  scores.control <- scores(simulated_AEs.control)
  scores.treatment <- scores(simulated_AEs.treatment)
  groups <- factor(c(rep("control", length(scores.control)), rep("treatment", length(scores.treatment))))
  score_data <- data.frame(scores = c(scores.control, scores.treatment), groups = groups)
  score_list <- c(score_list, list(score_data))
  if (index %% 20 == 0) {
    print(index / n_sim)
  }
}

print("running models")
anova_df <- data.frame()
for (scores_ in score_list) {
  anova_results <- run_anova(scores_)
  anova_df <- rbind(anova_df, anova_results)
}

tweedie_df <- data.frame()
for (scores_ in score_list) {
  tweedie_results <- run_tweedie(scores_)
  tweedie_df <- rbind(tweedie_df, tweedie_results)
}

quantile_df <- data.frame()
for (scores_ in score_list) {
  qauntile_regression_results <- run_qauntile_regression(scores_)
  quantile_df <- rbind(quantile_df, qauntile_regression_results)
}

lm_df <- data.frame()
for (scores_ in score_list) {
  lm_results <- run_lm(scores_)
  lm_df <- rbind(lm_df, lm_results)
}

df <- tibble(anova = anova_df, lm = lm_df, tweedie = tweedie_df, quantile = quantile_df, scores = score_list)

saveRDS(df, file = paste0("data/", scenario$name, ".rds"))
df.1 <- load_data(scenario)

simulate_scenario(scenario, n_sim = 100)
df <- load_data(scenario)