source("R/models/models.R")
source("R/trials/trial_loader.R")
x <- source("R/evaluation/distribution_fit/x.R")$value

trial_data <- load_longer_trials()


scenario_name <- trial_data$name
result_directory <- paste0("results/CDF/", scenario_name, "/")

model <- ZERO_INFLATED_GAMMA()
CDFS <- trial_data$apply_to_each(function(trial) model$fit(trial)$get_CDFs(x))

control_densities <- lapply(CDFS, function(CDF) CDF$control)
treatment_densities <- lapply(CDFS, function(CDF) CDF$treatment)

Y_control <- trial_data$all_data()[trial_data$all_data()$Group == "control", 1]
Y_treatment <- trial_data$all_data()[trial_data$all_data()$Group == "treatment", 1]


dir.create(result_directory, recursive = TRUE, showWarnings = FALSE)
file_path <- paste0(result_directory, model$repr, ".RData")

save(control_densities, treatment_densities, Y_control, Y_treatment, x,
     file = file_path)

load(file_path)

source("R/evaluation/analysis_and_comparison/distribution_plot.R")


plot_path <- paste0("plots/model_distributions/", scenario_name, "/")
dir.create(plot_path, recursive = TRUE, showWarnings = FALSE)

p <- distributions_plot(control_densities, x, Y_control, paste0("Log-Anova Distributions"))
ggsave(paste0(plot_path, model$repr, "_control.pdf"), plot = p, width = 8, height = 5)

p <- distributions_plot(treatment_densities, x, Y_treatment, "Log-Anova Distributions")

ggsave(paste0(plot_path, model$repr, "_treatment.pdf"), plot = p, width = 8, height = 5)


f <- function() {
  makeActiveBinding("a", i <- i + 0, env = GlobalEnv)
}

f <- function() {
  i <- 0
  makeActiveBinding("counter", function(a = NULL) {
    i <<- i + 0.5
    if (!is.null(a)) {i <<- a}
    return(i)
  }
    , env = .GlobalEnv)
}

f()
counter

counter = 0
counter
counter = 10
