# Load required functions

source("R/trials/trial_loader.R")
source("R/models/density_methods.R")
source("R/models/model_coefficients.R")
source("R/models/fit_models.R")



model_name <- "anova"
Distribution_plot_name <- "ANOVA Distributions"

model_dep_params <- list("tweedie"= list(var_power="infer") , "log_anova" = list(c=1))

model_fit_f <-  get_model_fit_f(model_name , args = model_dep_params[[model_name]])
n_it <- 500

filename <- paste0("data/workspaces/densities/",model_name,"RData")
if(!file.exists(filename)){
control_distributions <- list()
treatment_distributions <- list()

x =  c(seq(-50,30,by=0.2),
       seq(31,50,by=0.5),
       seq(51,100, by=1),
       seq(100,600,by=5))

trial_data <- load_longer_trials()


for(i in seq_along(trial_data$trials)){
  if(i > n_it){ break }
  trial <- trial_data$trials[[i]]
  model <- model_fit_f(trial)
  print(class(model))
  extract_coefficients(model)
  coefs <- split_model_coefficients(extract_coefficients(model))
  control_distributions[[i]] <- do.call(model_distribution, modifyList(coefs$control,list(model=model, x=x)) )
  treatment_distributions[[i]] <- do.call(model_distribution, modifyList(coefs$treatment,list( model=model , x=x )) )
}
  save.image(file = filename)
}else{
  load(filename)
}


all_data <- trial_data$all_data()
treatment_scores <- all_data[all_data$Group=="treatment",1]
control_scores <- all_data[all_data$Group=="control",1]


source("R/models/analysis_and_comparison/density_plot.R")

p <- density_plot(control_distributions,x, control_scores ,Distribution_plot_name )
p
ggsave(paste0(model_name,"_distributions_control.pdf"), plot = p, width = 8, height = 5)

p <- density_plot(treatment_distributions,x, treatment_scores , Distribution_plot_name )
p
ggsave(paste0(model_name,"_distributions_treatment.pdf"), plot = p, width = 8, height = 5)


