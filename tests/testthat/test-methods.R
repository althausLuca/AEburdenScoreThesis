
test_that("compilation without errors", {
  setwd("../..")
  source("R/models/fit_models.R", local = TRUE)
  source("R/models/model_CDFs.R", local = TRUE)

  source("tests/test_data.R", local = TRUE)
  models <- source("R/models/models.R", local = TRUE)$value

  for (model_f in models) {
    model <- model_f()
    expect_no_error(fitted_model <- fit_model(model , test_trial), message  = paste0("Error in ", model$name ))
    expect_no_error(model_distribution(fitted_model,x = seq(-1,100,by=0.1) ), message  = paste0("Error in ", model$name ))
  }
})

