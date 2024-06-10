library(tidyverse)
source("R/AdverseEvents/AdverseEvents.R")

n_sim <- 1000

SCENARIOS <- list(
  equal = list(
    name = "Equal",
    description = "All scenarios are equally likely, one AE with low probability of happenind",
    observations = 100,
    control_AEs = list(AE(2, 6),
                       AE( 5, 20),
                       AE( 3, 20)),

    treatment_AEs = list(AE( 1, 6),
                         AE( 5, 20),
                         AE( 3, 20))
  ),
  frequency = list(
    name = "Frequency",
    observations = 100,
    description = "More frequent events with treatment",
    control_AEs = list(AE( 2, 6),
                       AE( 5, 20),
                       AE( 3, 20)),
    treatment_AEs = list(AE( 2, 3),
                         AE( 5, 16),
                         AE( 3, 10))
  ),
  duration = list(
    name = "Duration",
    observations = 100,
    description = "Event duration is longer with treatment",
    control_AEs = list(AE( 2, 10),
                       AE( 5, 10),
                       AE( 10, 10)),
    treatment_AEs = list(AE( 3, 10),
                         AE( 8, 10),
                         AE( 15, 10))
  ),
  severity = list(
    name = "Severity",
    observations = 100,
    description = "A severe event that is unlikely to happen without treatment while both other events are low",
    treatment_AEs = list(AE( 3, 6, severity_probability = SEVERITY_PROBABILITIES$low),
                       AE( 5, 20, severity_probability = SEVERITY_PROBABILITIES$medium),
                       AE( 10, 20, severity_probability = SEVERITY_PROBABILITIES$high)),
    control_AEs = list(AE( 3, 6, severity_probability = SEVERITY_PROBABILITIES$low),
                         AE( 5, 20, severity_probability = SEVERITY_PROBABILITIES$low),
                         AE( 10, 20, severity_probability = SEVERITY_PROBABILITIES$medium))

  )
)

#just for testing
TESTSCENARIO <- list(
  name = "test",
  observations = 10,
  description = "jsut for me for testing",
  control_AEs = list(AE( 3, 20),
                     AE( 100, 20),
                     AE( 3, 20, severity_probability = SEVERITY_PROBABILITIES$high)),
  treatment_AEs = list(AE( 3, 20),
                       AE( 100, 20),
                       AE( 3, 20, severity_probability = SEVERITY_PROBABILITIES$high))
)

# #' Simulate a scenario
# #'
# #' This function simulates a scenario and saves the results to a json file.
# #'
# #' @param scenario A scenario object from SCENARIOS
# #' @param n_sim Number of simulations to run
# #'
# #' Stores the results in a rds file in the data directory with the name of the scenario
# simulate_scenario <- function(scenario, n_sim = n_sim) {
#   print(scenario$name)
#   print("generating scores")
#
#   score_list <- list()
#   for (index in 1:n_sim) {
#     simulated_AEs.control <- simulate_events(scenario$control_AEs, max_time = 180, n_it = scenario$observations)
#     simulated_AEs.treatment <- simulate_events(scenario$treatment_AEs, max_time = 180, n_it = scenario$observations)
#     scores.control <- scores(simulated_AEs.control)
#     scores.treatment <- scores(simulated_AEs.treatment)
#     groups <- factor(c(rep("control", length(scores.control)), rep("treatment", length(scores.treatment))))
#     score_data <- data.frame(scores = c(scores.control, scores.treatment), groups = groups)
#     score_list <- c(score_list, list(score_data))
#     if (index %% 20 == 0) {
#       print(index / n_sim)
#     }
#   }
#
#   print("running models")
#   anova_df <- data.frame()
#   for (scores_ in score_list) {
#     anova_results <- run_anova(scores_)
#     anova_df <- rbind(anova_df, anova_results)
#   }
#
#   tweedie_df <- data.frame()
#   for (scores_ in score_list) {
#     tweedie_results <- run_tweedie(scores_)
#     tweedie_df <- rbind(tweedie_df, tweedie_results)
#   }
#
#   quantile_df <- data.frame()
#   for (scores_ in score_list) {
#     qauntile_regression_results <- run_qauntile_regression(scores_)
#     quantile_df <- rbind(quantile_df, qauntile_regression_results)
#   }
#
#   lm_df <- data.frame()
#   for (scores_ in score_list) {
#     lm_results <- run_lm(scores_)
#     lm_df <- rbind(lm_df, lm_results)
#   }
#
#   df <- tibble(anova = anova_df, lm = lm_df, tweedie = tweedie_df, quantile = quantile_df, scores = score_list)
#
#   saveRDS(df, file = paste0("data/", scenario$name, ".rds"))
# }
#

#' Load a scenario
#' This function loads a scenario from a json file.
#' @param scenario A scenario object
#' @return A tibble dataframe with the results
#' @seealso \code{\link{simulate_scenario}} for simulating a scenario
#' @examples
#' scenario <- SCENARIOS$equal
#' df <- load_data(scenario)
load_data <- function(scenario) {
  data <- readRDS(file = paste0("data/", scenario$name, ".rds"))
  scores <- data$scores
  # I had a type in the data so this is a hack to fix it
  # iterate scores and for each df replace the group treament with treatment

  # Iterate over each dataframe in scores and correct the typo in the group name
  scores <- lapply(scores, function(df) {
    if("treament" %in% levels(df$groups)){
      df$groups <- factor(df$groups, levels = c(levels(df$groups), "treatment"))
      df$groups[df$groups == "treament"] <- "treatment"
    }
    return(df)
  })

  # Update the scores in data with the corrected version
  data$scores <- scores

  return(data)
}
