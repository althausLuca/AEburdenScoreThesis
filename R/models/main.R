source("R/models/models.R")
source("R/models/fit_models.R")
source("R/models/model_coefficients.R")
source("R/models/model_metrics.R")

test_data <- data.frame(
  Score = c(
    0, 0, 0, 0, 0, abs(rnorm(10, 5, 1)),
    0, 0, 0, abs(rnorm(12, 10, 1))
  ),
  Group = c(rep("control", 15), rep("treatment", 15))
)


# Run the models

model <- ANOVA()
model <- fit_model(model,test_data)
extract_coefficients(model)
extract_metrics(model)

model <- LOG_ANOVA(delta=0.001)
model <- fit_model(model,test_data)
extract_coefficients(model)
extract_metrics(model)

model <- LOG_ANOVA(delta=1)
model <- fit_model(model,test_data)
extract_coefficients(model)
extract_metrics(model)

model <- TWEEDIE_REGRESSION()
model <- fit_model(model,test_data)
extract_coefficients(model)
extract_metrics(model)


model <- QUANTILE_REGRESSION()
model <- fit_model(model,test_data)
extract_coefficients(model)
extract_metrics(model)

model <- WILCOXON_TEST()
model <- fit_model(model,test_data)
extract_coefficients(model)
extract_metrics(model)

model <- PERMUTATION_TEST()
model <- fit_model(model,test_data)
extract_coefficients(model)
extract_metrics(model)



