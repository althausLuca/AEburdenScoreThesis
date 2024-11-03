run_test.wilcoxon_test <- function(test, trial) {

  wt <- wilcox.test(trial$Score ~ trial$Group)
  p_value <- wt$p.value

  return(create_test_result(test, p_value))
}