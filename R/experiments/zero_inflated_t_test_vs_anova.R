source("R/trials/trial_loader.R")
source("R/models/models.R")

trial_data <- load_equal_trials()

anova_model <- ANOVA()
zero_inflated_t_test <- ZERO_INFLATED_TTEST()
zero_inflated_wilcoxon <- ZERO_INFLATED_WILCOXON_TEST()
fit_zero_inflated_lognormal_model <- ZERO_INFLATED_LOGNORMAL()


trial <- trial_data$trials[[1]]

run_tests <- function(trial){
    anova_p_value <- anova_model$fit(trial)$p_value
    zero_inflated_t_test_p_value <- zero_inflated_t_test$test(trial)$p_value
    zero_inflated_wilxon_p_value <- zero_inflated_wilcoxon$test(trial)$p_value
    return(list("anova" = anova_p_value,
                "zi_t_test" =  zero_inflated_t_test_p_value,
                "zi_wilcoxon_test" = zero_inflated_wilxon_p_value))
}


result_df <- trial_data$apply_to_each(run_tests , as.df = TRUE)

source("R/evaluation/analysis_and_comparison/p_value_plot.R")

p_handler <- p_value_plot_handler()
p_handler$add(unlist(result_df$anova), anova_model$name, "blue")
p_handler$add(unlist(result_df$zi_t_test), zero_inflated_t_test$name, "red")
p_handler$add(unlist(result_df$zi_wilcoxon_test), zero_inflated_wilcoxon$name, "orange")
p_handler$add(unlist(trial_data$apply_to_each(function(trial) fit_zero_inflated_lognormal_model$fit(trial)$p_value)),
                     fit_zero_inflated_lognormal_model$name, "green")

p_handler$plot()