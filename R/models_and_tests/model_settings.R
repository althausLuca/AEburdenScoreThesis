source("R/helpers.R")


pre_process_name <- function(model_name) {
  if (grepl("tweedie", model_name)) {
    model_name <- "tweedie"
  }
  if (grepl("quantile", model_name)) {
      model_name <- "quantile_regression"
  }
  if (grepl("permutation", model_name)) {
    model_name <- "permutation_test"
  }
  return(model_name)
}


get_color <- function(model_name) {
    model_name <- pre_process_name(model_name)
  color <- switch(model_name,
                  log_anova = "navy",
                  anova = "darkgreen",
                  tweedie = "tomato4",
                  quantile_regression = "lightpink4",
                  permutation_test = "yellow3",
                  wilcoxon = "purple",

                  ## log anova models
                  log_anova_c_0.001 = "royalblue4",
                  log_anova_c_1 = "navy",
                  log_anova_c_10000 = "royalblue1",

                  ## Zero inflated models
                  zero_inflated_ttest = "red",
                  zero_inflate_ttest = "red",
                  zero_inflated_gamma = "red4",
                  zero_inflated_lognormal = "maroon1",
                  "black"
  )

  return(unname(color))
}

get_marker <- function(model_name) {
  model_name <- pre_process_name(model_name)
  shape <- switch(model_name,
                       log_anova = 1,
                       anova = 2,
                       tweedie = 3,
                       quantile_regression = 4,
                       permutation_test = 5,
                       wilcoxon = 6,
                       log_anova_c_0.001 = 1,
                       log_anova_c_1 = 2,
                       log_anova_c_10000 = 3,

                       ## Zero inflated models
                       zero_inflated_ttest = 4,
                       zero_inflate_ttest = 4,
                       zero_inflated_gamma = 5,
                       zero_inflated_lognormal = 6,
                       1
  )

  return(unname(shape))
}



get_line_style <- function(model_name) {
  model_name <- pre_process_name(model_name)
  line_style <- switch(model_name,
                  log_anova = "dotdash",
                  anova = "dashed",
                  tweedie = "dotted",
                  quantile_regression = "solid",
                  permutation_test = "dashed",
                  wilcoxon = "dashed",
                  log_anova_c_0.001 = "dotdash",
                  log_anova_c_1 = "dotted",
                  log_anova_c_10000 = "1F",

                  ## Zero inflated models
                  zero_inflated_ttest = "dotted",
                  zero_inflate_ttest = "dotted",
                  zero_inflated_gamma = "dotdash",
                  zero_inflated_lognormal = "dashed",
                  "solid"
  )

  return(unname(line_style))
}


# Return Label for latex expressions using library(latex2exp)
map_labels <- function(model_name) {
  model_name <- pre_process_name(model_name)
  model_labels <- c(
    anova = "ANOVA",
    tweedie = "Tweedie Regression",
    quantile_regression = "Median Regression",
    log_anova_c_0.001 = "Log-ANOVA $$$_{c=0.001}$",
    log_anova_c_1 = "Log-ANOVA $$$_{c=1}$",
    log_anova_c_10000 = "Log-ANOVA $$$_{c=10000}$",
    permutation_test = "Permutation Test",
    wilcoxon = "Wilcoxon Test",
    zero_inflated_gamma = "Zero-Inflated Gamma",
    log_anova = "Log-ANOVA",
    zero_inflated_ttest = "Two-Part T-Test",
    zero_inflate_ttest = "Zero-Inflated T-Test",
    zero_inflated_lognormal = "Zero-Inflated Lognormal"
  )
  if (model_name %in% names(model_labels)) {
    return(model_labels[[model_name]])
  } else {
    return(model_name)
  }
}








