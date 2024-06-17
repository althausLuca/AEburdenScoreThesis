library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)

# Define the rescale_x function
rescale_x <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Define the map_labels function
map_labels <- function(x) {
  model_labels <- c(
    ANOVA = "$ANOVA$",
    tweedie = "Tweedie Regression",
    quantile_regression = "Median Regression",
    anova_c_0.001 = "Log-ANOVA $$$_{c=0.001}$",
    anova_c_1 = "Log-ANOVA $$$_{c=1}$"
  )
  if (x %in% names(model_labels)) {
    return(model_labels[[x]])
  } else {
    return(x)
  }
}

# Define the plot handler function
p_value_plot_handler <- function() {
  data <- data.frame()
  model_names <- c()
  colors <- c()

  add <- function(model_p_values, model_name, color = "black") {
    model_p_values <- sort(model_p_values)
    x <- rescale_x(1:length(model_p_values))
    stopifnot(x>=0 & x<=1)
    model_names <<- c(model_names, model_name)
    colors <<- c(colors, color)

    model_data <- data.frame(
      p_value = model_p_values,
      rescaled_id = x,
      model = model_name
    )
    data <<- rbind(data, model_data)
  }

  plot <- function() {
    ggplot(data, aes(x = p_value, y = rescaled_id, group = model)) +
      geom_line(aes(color = model, linetype = model), size = 1.1) +
      labs(x = "P-Value", y = "CDF", title = "") +
      theme_minimal() +
      theme(
        legend.position = c(0.99, 0.01),
        legend.justification = c(1, -0.1),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.box.just = "left",
        axis.text = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 16, face = "bold")
      ) +
      scale_color_manual(values = colors[order(model_names)],
                         labels = lapply(sort(model_names), function(x) TeX(map_labels(x)))) +
      scale_linetype_manual(values = setNames(1:length(model_names), model_names),
                            labels = lapply(sort(model_names), function(x) TeX(map_labels(x)))) +
      geom_vline(xintercept = 0.05, linetype = "dashed", color = "red") +
      annotate("text", x = 0.1, y = -0, label = "0.05", angle = 0, color = "red")+
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")
  }

  return(list(plot = plot, add = add))
}


source("R/methods/run_methods.R")
source("R/methods/methods_settings.R")

#load("perm_test_longer.RData")
#model_file <- "Scenario_3_k_1.5_l_3.5.csv"

#load("perm_test_shorter.RData")
#model_file <- "Scenario_2_k_1.5_s_0.5.csv"




p_values <- get_values(model_file, "p_value")

names(p_values)

# Example usage
handler <- p_value_plot_handler()
handler$add(p_values$lm, "ANOVA", "darkgreen")
handler$add(p_values$anova_c_0.001, "Log-ANOVA $$$_{c=0.001}$", "navy")
handler$add(p_values$anova_c_1, "Log-ANOVA $$$_{c=1}$", "royalblue4")
handler$add(p_values$tweedie, "Tweedie Regression", "blue")
handler$add(p_values$quantile_regression, "Median Regression", "orange")


handler$add(permutations_tests, "Permutation Test", "red")
handler$plot()

print(plot)


load("R/trials/trial_loader.R")


#model_file <- "Scenario_2_k_1.5_s_0.5.csv"
model_file <- "Scenario_1_k_1.5.csv"

trial_data <- load_trial_data(model_file)

group_size <- 20

anova_30 <- function(trial){
  trial <- trial[c(1:group_size,101:(100+group_size)),]
  lm_model <- lm(Score ~ Group, data = trial)
  summary <- summary(lm_model)
  p_value <- summary$coefficients[2, 4]
  return(p_value)
}

p_values_30 <- unlist(trial_data$apply_to_each(anova_30))

handler <- p_value_plot_handler()

handler$add(p_values_30, paste0("Anova ",group_size), "red")
handler$plot()

trial_1 <- trial_data$trials[[1]]
trial_1 <- trial_1[c(1:10,101:110),]
lm_model <- lm(Score ~ Group, data = trial_1)
summary(lm_model)

