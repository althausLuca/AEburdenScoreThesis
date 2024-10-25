source("R/trials/trial_loader.R")
source("R/models_and_tests/models_and_tests.R")
source("R/models_and_tests/models/zero_inflated_normal.R")
# trial_data <- load_longer_trials()
trial <- trial_data$trials[[1]]
fit <- fit_model.zero_inflated_normal(, trial)

plot_CDFs(fit, trial)


source("R/evaluation/plot_functions/distribution_plot.R")



fit_model.anova_model(, trial)$AIC
