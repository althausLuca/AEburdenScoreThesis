source("R/helpers.R")



get_color <- function(model_name) {
  color <- switch(model_name,
                  log_anova = "navy",
                  anova = "darkgreen",
                  tweedie = "blue",
                  quantile_regression = "orange",
                  permutation_test = "darkred",
                  wilcoxon = "purple",
                  anova_c_0.001 = "royalblue4",
                  anova_c_1 = "navy",
                  log_anova_c_0.001 = "royalblue4",
                  log_anova_c_1 = "navy",
                  "black"
  )
}

# Return Label for latex expressions using library(latex2exp)
map_labels <- function(x) {
  model_labels <- c(
    anova = "ANOVA",
    tweedie = "Tweedie Regression",
    quantile_regression = "Median Regression",
    log_anova_c_0.001 = "Log-ANOVA $$$_{c=0.001}$",
    log_anova_c_1 = "Log-ANOVA $$$_{c=1}$",
    permutation_test = "Permutation Test" ,
    wilcoxon = "Wilcoxon Test"
  )
  if (x %in% names(model_labels)) {
    return(model_labels[[x]])
  } else {
    return(x)
  }
}








