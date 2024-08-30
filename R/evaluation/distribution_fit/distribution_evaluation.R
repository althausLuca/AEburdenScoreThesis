# Load required functions
source("R/trials/trial_loader.R")
source("R/models_and_tests/models_and_tests.R")


trial_data <- load_shorter_trials()
MODEL <- LOG_ANOVA(delta = 1)

Distribution_plot_name <- "Tweedie Distributions"
filepath <- "data/distributions/shorter/"
plot_path <- "plots/model_distributions/shorter/"

dir.create(filepath, recursive = TRUE)
dir.create(plot_path, recursive = TRUE)

filename <- paste0(filepath,MODEL$repr ,".RData")


n_it <- 5000
recompute <- TRUE


if(!file.exists(filename) || recompute ){
  control_distributions <- list()
  treatment_distributions <- list()

  x <-  c(seq(-50,30,by=0.2),
         seq(31,50,by=0.5),
         seq(51,100, by=1),
         seq(100,600,by=5))


  for(i in seq_along(trial_data$trials)){
    if(i > n_it){ break }
    trial <- trial_data$trials[[i]]
    model <- fit_model(MODEL, trial)
    coefs <- split_model_coefficients(extract_coefficients(model))
    control_distributions[[i]] <- do.call(model_distribution, modifyList(coefs$control,list(model=model, x=x)) )
    treatment_distributions[[i]] <- do.call(model_distribution, modifyList(coefs$treatment,list( model=model , x=x )) )
  }
  save.image(file = filename)
} else{
  tmp <- Distribution_plot_name #might  have changed this
  load(filename)
  Distribution_plot_name <- tmp
}


all_data <- trial_data$all_data()
treatment_scores <- all_data[all_data$Group=="treatment",1]
control_scores <- all_data[all_data$Group=="control",1]


source("../../models/analysis_and_comparison/distribution_plot.R")
p <- distributions_plot(control_distributions, x, control_scores , Distribution_plot_name )
ggsave(paste0(plot_path,MODEL$repr,"_distributions_control.pdf"), plot = p, width = 8, height = 5)



library(profvis)
library(svglite)

source("../../models/analysis_and_comparison/distribution_plot.R")

profvis({
  p <- distributions_plot(treatment_distributions, x, treatment_scores , Distribution_plot_name )
  ggsave(paste0(plot_path,MODEL$repr,"_distributions_treatment_.svg"), plot = p, width = 8, height = 5)
})


