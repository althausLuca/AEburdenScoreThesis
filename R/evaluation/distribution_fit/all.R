source("R/models_and_tests/models_and_tests.R")
source("R/trials/trial_loader.R")
source("R/evaluation/distribution_fit/model_cdfs_plots.R")
library(pracma)


get_x_range <- function(model){
  print(model$repr)
  pos_log <- c(0,pracma::logseq(0.1, 600, n = 90))
  default <- c(-30, pos_log)
  repr <- model$repr
  if(LOG_ANOVA(c = 0.001)$repr == repr){
    return(c(-30, -0.001, seq( -0.001, 0, length.out = 5), pos_log))
  }
  if(LOG_ANOVA(c = 1)$repr == repr){
        return(c(-30, -1, seq( -1, 0, length.out = 5), pos_log))
  }
  if(ANOVA()$repr == repr || ZERO_INFLATED_NORMAL()$repr == repr){
        return(c(seq( -30, 0, length.out = 40), pos_log))
  }
  return(default)
}


models <- list(
                # ZERO_INFLATED_NORMAL() ,
                # ANOVA(),
                # LOG_ANOVA(c = 0.001),
                # LOG_ANOVA(c = 1)
                TWEEDIE_REGRESSION(xi = "infer"),
                ZERO_INFLATED_LOGNORMAL(),
                ZERO_INFLATED_GAMMA()
)


for( scenario in names(scenario_loaders)){
  trial_data <- scenario_loaders[[scenario]]()
  for(model in models){
    x <<- get_x_range(model)
    plots <- (model_cdf_plots(model, trial_data, scenario , limit = 4000))
  }
}

plot_path <- "plots/model_distributions/"
ggsave(paste0(plot_path, model$repr, "_treatment.png"), plot = plots[[1]], width = 8*3, height = 5*3 , units="cm", dpi=1000)
