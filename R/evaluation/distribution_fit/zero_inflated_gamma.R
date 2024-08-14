source("R/models/models.R")
scenario <- "longer"

model_name.1 <- ZERO_INFLATED_GAMMA(sigma_per_group = TRUE)$repr
model_name.2 <- ZERO_INFLATED_GAMMA(sigma_per_group = FALSE)$repr

model_name <- model_name.2
data_path <- paste0("data/distributions/",scenario, "/")
plot_path <- paste0("plots/distributions/", scenario, "/")


file.1 <- paste0(data_path, "/", model_name.1, ".RData")
file.2 <- paste0(data_path, "/", model_name.2, ".RData")

load(file.1)
# distribution_results.1 <- distribution_results
load(file.2)
# distribution_results.2 <- distribution_results


names(distribution_results)

class(distribution_results$control_results)

all_data <- distribution_results$trial_data$all_data()

source("R/models/analysis_and_comparison/distribution_plot.R")

control_data <- all_data[all_data$Group == "control", 1]

p_control <- distributions_plot(t(distribution_results$control_results), distribution_results$x,control_data, "Control", "ECDF" ,text_size_factor=1)

treatment_data <- all_data[all_data$Group == "treatment", 1]
p_treatment <- distributions_plot(t(distribution_results$treatment_results), distribution_results$x, treatment_data, "Treatment", "ECDF",text_size_factor=1)

# store the plots as pdf
ggsave(paste0(plot_path, model_name, "_distributions_control_f.pdf"), plot = p_control, width = 10 , height = 8)
ggsave(paste0(plot_path, model_name, "_distributions_treatment_f.pdf"), plot = p_treatment, width = 10, height = 8)

