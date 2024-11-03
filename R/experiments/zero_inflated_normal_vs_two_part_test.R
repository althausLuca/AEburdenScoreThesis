source("R/models_and_tests/models_and_tests.R")
source("R/trials/trial_loader.R")
source("R/evaluation/plot_functions/p_value_plot.R")


t_test <- TWO_PART_T_TEST()
zi_model <- ZERO_INFLATED_NORMAL()


fuc <- function(trial){
    lrt_p_value <- fit_model(zi_model, trial)$p_value
    two_part_test_p_value <- run_test(t_test, trial)$p_value
  return(list(lrt_p_value = lrt_p_value, two_part_test_p_value = two_part_test_p_value))
}

for(scenario in names(default_scenario_loaders)){
  trial_data <- default_scenario_loaders[[scenario]]()

  results <- trial_data$apply_to_each(fuc, as.df = TRUE)

  plot_handler <- p_value_plot_handler()

  plot_handler$add(results$lrt_p_value, "ZI-Norm LRT" , color="red")
  plot_handler$add(results$two_part_test_p_value, "Two-part T-Test")
  plot_handler$plot(infer_colors = FALSE)
  plot_handler$save(paste0("ZI_LRT_TWO_PART_T_", scenario , ".pdf") ,  infer_colors = FALSE)
}