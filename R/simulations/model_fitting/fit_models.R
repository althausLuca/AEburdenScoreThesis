source("R/trials/trial_loader.R")


print("running models")

result_folder <- "data/models/"

trial_data <- load_longer_trials()

source("R/models/run_models.R")
res <- run_models_from_trial_data(trial_data , use_parallel = TRUE)


# print(model_result_list)
#
save.image(paste0(result_folder, "longer_trials_l.RData"))