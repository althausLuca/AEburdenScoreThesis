source("../../Scenarios.R")
source("../methods.R")


scenario <- SCENARIOS$equal
data <- load_data(scenario)
sum(data$anova$p_value[data$tweedie$p_value > 0.05] < 0.05)
sum(data$tweedie$p_value[data$anova$p_value > 0.05] < 0.05)

scenario <- SCENARIOS$severity
data <- load_data(scenario)
sum(data$anova$p_value[data$tweedie$p_value < 0.05] > 0.05)
sum(data$tweedie$p_value[data$anova$p_value < 0.05] > 0.05)

scenario <- SCENARIOS$duration
data <- load_data(scenario)
sum(data$tweedie$p_value > 0.05)
sum(data$tweedie$p_value > 0.05)
sum(data$anova$p_value[data$tweedie$p_value < 0.05] > 0.05)
sum(data$tweedie$p_value[data$anova$p_value < 0.05] > 0.05)

scenario <- SCENARIOS$frequency
data <- load_data(scenario)
sum(data$tweedie$p_value > 0.05)
sum(data$tweedie$p_value > 0.05)
sum(data$anova$p_value[data$tweedie$p_value < 0.05] > 0.05)
sum(data$tweedie$p_value[data$anova$p_value < 0.05] > 0.05)