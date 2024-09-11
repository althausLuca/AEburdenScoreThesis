source("R/trials/trial_loader.R")
source("R/models_and_tests/models_and_tests.R")
source("R/models_and_tests/model_computer.R")

model_computer_file <- "results/more_severe_events/default.RData.RData"
model_computer <- load_model_computer(model_computer_file)
trial_data <- load_equal_trials()
summary(trial_data)


trial_data_default <- model_computer$trial_data
summary(trial_data_default)
test <- TWO_PART_WILCOXON_TEST()

test_  <- function(trial) run_test(test, trial)$p_value

results <- trial_data_default$apply_to_each(test_)

mean(unlist(results) < 0.05)
#[1] 0.8614
#[1] 0.5236

mean(model_computer$model_metrics$two_part_wilcoxon$p_value < 0.05)
model_computer$file_path
source("R/simulations/default_models.R")
add_models(model_computer,  DEFAULT_MODELS , recompute=TRUE)