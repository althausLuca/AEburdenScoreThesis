source("R/models_and_tests/models_and_tests.R")
source("R/trials/trial_loader.R")

model <- QUANTILE_REGRESSION(tau=0.75)

extract_different_p_values <- function(fit_) {
  model_names <- names(fit_)
  p_value_names <- grep("p_value", model_names, value = TRUE)
  print(fit_[p_value_names])
  return(fit_[p_value_names])
}


## shorter trials
trial_data <- load_shorter_trials()
p_values_shorter <- trial_data$apply_to_each(function(trial) extract_different_p_values(fit_model(model, trial)), as.df = TRUE)
sink()

sig_rate <- colMeans(p_values_shorter < 0.05, na.rm = TRUE)
na_count <- colSums(is.na(p_values_shorter))

# Combine into a single data frame
result_df_shorter <- data.frame(
  "Significance Rate (%)" = sig_rate,
  "NA Count" = na_count
)


## equal trials
trial_data <- load_equal_trials()
p_values_equal <- trial_data$apply_to_each(function(trial) extract_different_p_values(fit_model(model, trial)), as.df = TRUE)
sink()

sig_rate <- colMeans(p_values_equal < 0.05, na.rm = TRUE)
na_count <- colSums(is.na(p_values_equal))

# Combine into a single data frame
result_df_equal <- data.frame(
  "Significance Rate (%)" = sig_rate,
  "NA Count" = na_count
)


## longer trials
trial_data <- load_longer_trials()
p_values_longer <- trial_data$apply_to_each(function(trial) extract_different_p_values(fit_model(model, trial)), as.df = TRUE)
sink()

sig_rate <- colMeans(p_values_longer < 0.05, na.rm = TRUE)
na_count <- colSums(is.na(p_values_longer))

# Combine into a single data frame
result_df_longer <- data.frame(
  "Significance Rate (%)" = sig_rate,
  "NA Count" = na_count
)


print("Equal Trials")
print(result_df_equal)

print("Shorter Gap Times Trials")
print(result_df_shorter)

print("Longer Event duration Trials")
print(result_df_longer)

#
# trial_data <- load_equal_trials()
#
# for (i in seq_along(trial_data$trials)) {
#   trial <- trial_data$trials[[i]]
#   fit <- fit_model(model, trial)
#   print(sum(trial[trial$Group == "control",]$Score == 0))
#   print(sum(trial[trial$Group == "treatment",]$Score == 0))
#
#   if (is.na(fit$nid_p_value)) {
#     stop(paste0("NID P Value is NA for trial:",i, "/n"))
#   }
# }