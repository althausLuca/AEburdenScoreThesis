run_test.wilcoxon_test <- function(test, trial) {
  wt <- wilcox.test(trial$Score ~ trial$Group)
  wt$p_value <- wt$p.value
  wt$p.value <- NULL
  return(wt)
}