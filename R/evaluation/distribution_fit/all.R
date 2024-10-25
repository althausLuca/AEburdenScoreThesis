source("R/models_and_tests/models_and_tests.R")
source("R/trials/trial_loader.R")
source("R/evaluation/distribution_fit/model_cdfs_plots.R")


models <- list(
                ANOVA(),
                LOG_ANOVA(c = 0.001),
                LOG_ANOVA(c = 1),
                TWEEDIE_REGRESSION(xi = 1.8 ),
                TWEEDIE_REGRESSION(xi = 1.8 , use_mle = T),
                TWEEDIE_REGRESSION(xi = "infer"),
                TWEEDIE_REGRESSION(xi = "infer", use_mle = T),
                ZERO_INFLATED_LOGNORMAL(),
                ZERO_INFLATED_LOGNORMAL(sigma_per_group = T),
                ZERO_INFLATED_GAMMA(),
                ZERO_INFLATED_GAMMA(sigma_per_group = T),
                ZERO_INFLATED_NORMAL(),
                ZERO_INFLATED_NORMAL(sigma_per_group = T)
)

for( scenario in names(default_scenario_loaders)){
  trial_data <- default_scenario_loaders[[scenario]]()
  for(model in models){
    x <<- get_x_range(model)
    plots <- (model_cdf_plots(model, trial_data, scenario , limit = 20))
  }
}

