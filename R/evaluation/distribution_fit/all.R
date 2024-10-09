source("R/models_and_tests/models_and_tests.R")
source("R/trials/trial_loader.R")
source("R/evaluation/distribution_fit/model_cdfs_plots.R")

#ZERO_INFLATED_LOGNORMAL(),ANOVA() , LOG_ANOVA(c = 0.001), LOG_ANOVA(c = 1) ,

model <- ZERO_INFLATED_NORMAL()
models <- list( model , ANOVA())#, ANOVA() , LOG_ANOVA(c = 0.001), LOG_ANOVA(c = 1) , TWEEDIE_REGRESSION(xi = 1.2),  TWEEDIE_REGRESSION(xi = 1.5),  TWEEDIE_REGRESSION(xi = 1.8)  )


for( scenario in names(scenario_loaders)){
  trial_data <- scenario_loaders[[scenario]]()
  for(model in models){
    print(model_cdf_plots(model, trial_data, scenario))
  }
}