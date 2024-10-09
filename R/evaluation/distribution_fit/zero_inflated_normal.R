source("R/trials/trial_loader.R")
source("R/models_and_tests/models_and_tests.R")
source("R/evaluation/distribution_fit/model_cdfs_plots.R")

trial_data <- load_longer_trials()
trial <- trial_data$trials[[1]]

model <- ZERO_INFLATED_NORMAL()

fit_model(model, trial)

Rprof("profile_output.txt")

plots <- model_cdf_plots(model, trial_data, "test")

Rprof(NULL)
summaryRprof("profile_output.txt")
