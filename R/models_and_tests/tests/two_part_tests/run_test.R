source("R/models_and_tests/tests/two_part_tests/two_part_methods.R")

run_test.two_part_t_test <- function(test, trial) {
  if (test$parameters$use_welch) {
    result <- two_part_test(trial, test = "welch")
  } else {
    result <- two_part_test(trial, test = "ttest")
  }
  print(result)
  p_value <- result$p_value
  return(create_test_result(test, p_value))
}

run_test.two_part_wilcoxon <- function(test, trial) {
  result <- two_part_test(trial, test = "wilcoxon")
  p_value <- result$p_value
  return(create_test_result(test, p_value))
}

