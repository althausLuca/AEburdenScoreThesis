source("R/trials/trial_loader.R")
source("R/models_and_tests/models_and_tests.R")
source("R/models_and_tests/model_computer.R")

model_computer_file <- "results/more_severe_events/default.RData.RData"
model_computer <- load_model_computer(model_computer_file)
trial_data <- load_equal_trials()

trial <- trial_data$trials[[1]]
treatment_scores <- trial$Score[trial$Group == "treatment"]
control_scores <- trial$Score[trial$Group == "control"]

t.test(trial$Score ~ trial$Group, alternative = "two.sided", var.equal = FALSE)$p.value #welch
t.test(control_scores, treatment_scores, alternative = "two.sided")$p.value #welch
t.test(control_scores, treatment_scores, alternative = "two.sided", var.equal = TRUE)$p.value #t-test
summary(lm(trial$Score ~ trial$Group))$coefficients[2,4]

anova(lm(trial$Score ~ trial$Group))

#t-test by hand
n_treatment <- length(treatment_scores)
n_control <- length(control_scores)

mean_treatment <- mean(treatment_scores)
mean_control <- mean(control_scores)

var_treatment <- var(treatment_scores)
var_control <- var(control_scores)

t <- (mean_treatment - mean_control) / sqrt(var_treatment / n_treatment + var_control / n_control)
p_value <- 2 * pt(-abs(t), df = n_treatment + n_control - 2)