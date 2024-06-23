

run_wilcoxon_test <- function(score_data) {
  w_test <- wilcox.test(score_data$Score ~ score_data$Group)
  p_value <- w_test$p.value
  return(list("model" = "wilcoxon_test" , p_value= p_value))
}

