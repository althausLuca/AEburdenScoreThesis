source("R/models_and_tests/tests/two_part_tests/two_part_methods.R")


run_test.two_part_t_test <- function(test, trial) {
  if (test$parameters$use_welch) {
    return(two_part_test(trial, test = "welch"))
  }
  return(two_part_test(trial, test = "ttest"))
}

run_test.two_part_wilcoxon <- function(test, trial) {
  return(two_part_test(trial, test = "wilcoxon"))
}

