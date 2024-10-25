source("R/models_and_tests/models_and_tests.R")

#model, latex_label, color, line_style, marker
default_models_to_plot <- list(
  list(WILCOXON_TEST(), "Wilcoxon Test", "darkgreen", "dashed", 6),
  list(PERMUTATION_TEST(), "Permutation Test", "yellow3", "dotted", 5),
  list(ANOVA(), "ANOVA", "purple", "solid", 5),
  list(LOG_ANOVA(c = 0.001), "Log-ANOVA $$$_{c=0.001}$", "royalblue4", "dotdash", 1),
  list(LOG_ANOVA(c = 1), "Log-ANOVA $$$_{c=1}$", "navy", "dotted", 2),
  list(TWEEDIE_REGRESSION(xi = "infer"), "Tweedie Regression", "tomato4", "dotted", 4),
  list(ZERO_INFLATED_GAMMA(), "Zero-Inflated Gamma", "red4", "dotdash", 5),
  list(ZERO_INFLATED_LOGNORMAL(), "Zero-Inflated Lognormal", "maroon1", "dashed", 6),
  list(TWO_PART_WILCOXON_TEST(), "Two-Part Wilcoxon Test", "orangered1", "1F", 1),
  list(TWO_PART_T_TEST(), "Two-Part T-Test", "red", "dotted", 4),
  list(QUANTILE_REGRESSION(tau = 0.5), "Median Regression", "lightpink4", "solid", 3),
  list(QUANTILE_REGRESSION(tau = 0.75), "Quantile Regression $$$_{\\tau=0.75}$", "lightpink1", "dotted", 1)
)


model_reprs <- sapply(default_models_to_plot, function(x) x[[1]]$repr)
model_labels <- sapply(default_models_to_plot, function(x) x[[2]])
model_colors <- sapply(default_models_to_plot, function(x) x[[3]])
model_line_styles <- sapply(default_models_to_plot, function(x) x[[4]])
model_markers <- sapply(default_models_to_plot, function(x) x[[5]])


get_label <- function(model_repr){
    model_index <- which(model_reprs == model_repr)
    if(length(model_index) == 0){
        print(paste0("Model ", model_repr, " not found"))
        return(model_repr)
    }
    return(model_labels[model_index])
}

get_color <- function(model_repr){
    model_index <- which(model_reprs == model_repr)
    if(length(model_index) == 0){
        print(paste0("Model ", model_repr, " not found"))
        return("black")
    }
    return(model_colors[model_index])
}

get_line_style <- function(model_repr){
    model_index <- which(model_reprs == model_repr)
    if(length(model_index) == 0){
        print(paste0("Model ", model_repr, " not found"))
        return("solid")
    }
    return(model_line_styles[model_index])
}

get_marker <- function(model_repr){
    model_index <- which(model_reprs == model_repr)
    if(length(model_index) == 0){
        print(paste0("Model ", model_repr, " not found"))
        return(1)
    }
    return(model_markers[model_index])
}