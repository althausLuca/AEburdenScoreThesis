#' This file is used to load some trial data, one  trial and the models and tests
#' to quickly try out/tests things
source("R/evaluation/config.R")
source("R/models_and_tests/models_and_tests.R")
source("R/run_models/model_computer.R")

model_computer <- load_model_computer(DEFAULT_DURATION_VAR_FILE)
sd_model <- model_computer$model_metrics$anova$std_err[[1]]
mean_control <- model_computer$model_estimates$anova[1,1][[1]]
mean_treatment <- model_computer$model_estimates$anova[1,2][[1]]


trial <- trial_as_df(model_computer$trial_data$trials[[18]])
model <- lm(Score ~ Group, data = trial)

score <- trial$Score
sd(score-fitted(model))
sd(score-fitted(model))*sqrt(199/198)
summary(model)$sigma

sd(c(score[trial$Group == "control"]-mean(score[trial$Group == "control"]),
  score[trial$Group == "treatment"]-mean(score[trial$Group == "treatment"])))*sqrt(199/198)


model <- lm(Score ~ Group, data = trial)
model$terms
sd <- summary(model)$sigma #0.000582

estim <- 14.982

sq <- sqrt(1/100 + 1/100)
pt(-estim/(sd*sq), lower.tail = TRUE, df = 198)


t.val <- 3.497
p.val <- pt(t.val, lower.tail = FALSE, df  = 199)

sd(model)
sd
score <- trial$

names(model_computer$models)

model <- ZERO_INFLATED_NORMAL()
add_model(model_computer, model,save=FALSE)
model_computer$model_metrics[[model$repr]]

model.2 <- ZERO_INFLATED_NORMAL(sigma_per_group = TRUE)
add_model(model_computer, model.2)

unlist(model_computer$model_metrics[[model.2$repr]]$AIC) - unlist(model_computer$model_metrics[[model$repr]]$AIC)
