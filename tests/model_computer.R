library(data.table)

#init function
init_model_computer <- function(trial_data, name = "model_computer") {
  result <- environment()
  name <- name
  class(result) <- "model_computer"

  save <- function() {
    base::save(result, file = paste0(name, ".RData"))
  }

  result$model_metrics <- list()
  result$trial_data <- trial_data
  result$models <- list()

  return(result)
}

save.model_computer <- function(model_computer , name= model_computer$name) {
  file_name <- paste0(name, ".RData")
  print(file_name)
  base::save(model_computer, file =file_name)
}

add_model <- function(model_computer, model_or_test , save=TRUE) {
  model_computer$models[[model_or_test$repr]] <- model_or_test
  if (inherits(model_or_test, "model")) {
    model_fits <- model_computer$trial_data$apply_to_each(model_or_test$fit, use_parallel = TRUE)
    model_restults <- data.table(t(sapply(model_fits, function(x) x$estimates)))
    model_restults$p_value <- lapply(model_fits, function(fit) fit$p_value)
  }
  else if (inherits(model_or_test, "test")) {
    test_fits <- model_computer$trial_data$apply_to_each(model_or_test$test, use_parallel = TRUE)
    p_values <- sapply(test_fits, function(x) x$p_value)
    model_restults <- data.table(p_value = p_values)
  }
  else {
    stop("model_or_test must be a model or test object")
  }
  model_computer$model_metrics[[model_or_test$repr]] <- model_restults

  if (save){
    save.model_computer(model_computer)
  }
}


get_value <- function(model_computer, val = "p_value") {
  result <- NULL
  for (model in names(model_computer$models)) {
    if (val %in% names(model_computer$model_metrics[[model]])) {
      if (is.null(result)) {
        result <- data.table(model_computer$model_metrics[[model]][[val]])
        setnames(result, model)
      }
      else {
        result[[model]] <- model_computer$model_metrics[[model]][[val]]
      }

    }
  }
  return(result)
}

library(profvis)

#profile the code

source("R/trials/trial_loader.R")
trial_data <- load_shorter_trials()
model_computer <- init_model_computer(trial_data, "test_test")

source("R/models/models.R")

profvis({
  model_or_test <- ANOVA()

  add_model(model_computer, model_or_test)
  get_value(model_computer, "p_value")

  model_or_test <- ZERO_INFLATED_GAMMA()
  add_model(model_computer, model_or_test)

  model_or_test <- WILCOXON_TEST()
  add_model(model_computer, model_or_test)

  model_or_test <- QUANTILE_REGRESSION()
  add_model(model_computer, model_or_test)
  save.model_computer(model_computer)

  model_or_test <- PERMUTATION_TEST()
  add_model(model_computer, model_or_test)
})

source("R/trials/trial_loader.R")
trial_data <- load_shorter_trials()
model_computer <- init_model_computer(trial_data, "test_test_ddd")
model_computer$trial_data$n_trials

source("R/models/models.R")
model_or_test <- ANOVA()

add_model(model_computer, model_or_test)
get_value(model_computer, "p_value")

model_or_test <- ZERO_INFLATED_GAMMA()
add_model(model_computer, model_or_test)

model_or_test <- WILCOXON_TEST()
add_model(model_computer, model_or_test)

model_or_test <- QUANTILE_REGRESSION()
add_model(model_computer, model_or_test)
save.model_computer(model_computer)

model_or_test <- PERMUTATION_TEST()
add_model(model_computer, model_or_test)

model_computer$name
a <- "b"
l <- list({
      a <- 3
      x <- 131
      y <- 5
      print("hallo")
})
y

{{
  d = 3
}}
d
