source("R/trials/trial_loader.R")
source("R/models_and_tests/tests/two_part_tests/two_part_methods.R")


trials_data_loaders <- list("equal" = load_equal_trials, "longer" = load_longer_trials, "shorter" = load_shorter_trials)

for (i in seq_along(trials_data_loaders)) {
  scenario <- names(trials_data_loaders)[[i]]
  trial_data <- trials_data_loaders[[i]]()


  plot_folder <- paste0("plots/two_part_scatter_plots/", scenario, "/")
  dir.create(plot_folder, showWarnings = FALSE, recursive = TRUE)

  test_wilcoxon <- function(trial) { return(two_part_test(trial, test = "wilcoxon")) }
  test_t <- function(trial) { return(two_part_test(trial, test = "ttest")) }
  B_T_P_wilcoxon <- trial_data$apply_to_each(test_wilcoxon, limit = 5000, as.df = TRUE)
  B_T_P_t <- trial_data$apply_to_each(test_t, limit = 5000, as.df = TRUE)

  B_T_P_wilcoxon$B <- unlist(B_T_P_wilcoxon$B)
  B_T_P_wilcoxon$T <- -unlist(B_T_P_wilcoxon$T)
  B_T_P_t$B <- unlist(B_T_P_t$B)
  B_T_P_t$T <- unlist(B_T_P_t$T)

  alpha <- 0.05
  # chi square alpha level 0.5 value with 2df
  chi_square_threshold <- qchisq(alpha, 2, lower.tail = FALSE)
  circle_radius <- sqrt(chi_square_threshold)

  pdf(paste0(plot_folder, "T_wilcoxon.pdf"), width = 9, height = 9)
  #scatter plot of B and T$

  x_lim <- c(min(B_T_P_wilcoxon$B,B_T_P_t$B) - 0.3, max(B_T_P_wilcoxon$B,B_T_P_t$B) + 0.3)
  y_lim <- c(min(B_T_P_wilcoxon$T,B_T_P_t$T) - 0.3, max(B_T_P_wilcoxon$T,B_T_P_t$T) + 0.3)

  plot(B_T_P_wilcoxon$B, B_T_P_wilcoxon$T, xlab = "B", ylab = "T", main = "",
       pch = 1, col = "blue"
    , asp = 1, xlim = x_lim, ylim = y_lim
    ,cex.axis = 1.2, cex.lab = 1.4)
  points(B_T_P_wilcoxon$B[B_T_P_t$p_value > alpha], B_T_P_wilcoxon$T[B_T_P_t$p_value > alpha],
         pch = 19, col = "blue")

  symbols(0, 0, circles = circle_radius, inches = FALSE, add = TRUE, fg = "black", lwd = 1)
  # text(-0.8, 1, "Region of rejection \nat sig. level of 0.05", col = "black", cex = 0.8)
  dev.off()

  pdf(paste0(plot_folder, "T_ttest.pdf"), width = 9, height = 9)

  #scatter plot of B and T
  plot(B_T_P_t$B, B_T_P_t$T, xlab = "B", ylab = "T",
       main = "", pch = 1, col = "blue", xlim = x_lim, ylim = y_lim
    , asp = 1, lwd = 0.3 ,cex.axis = 1.2, cex.lab = 1.4)
  points(B_T_P_t$B[B_T_P_wilcoxon$p_value > alpha], B_T_P_t$T[B_T_P_wilcoxon$p_value > alpha],
         pch = 19, col = "blue")

  symbols(0, 0, circles = circle_radius, inches = FALSE, add = TRUE, fg = "black", lwd = 1)
  # text(-1, 1, "Region of rejection \nat sig. level of 0.05", col = "black", cex = 0.8)

  dev.off()

  # Print scenario information
  print(paste0("scenario: ", scenario))

  # Wilcoxon Pearson correlation
  print("wilcoxon pearson corr")
  print(corr_w <- cor.test(B_T_P_wilcoxon$B, B_T_P_wilcoxon$T, method = "pearson"))

  # t-test Pearson correlation
  print("ttest pearson corr")
  print(corr_t <- cor.test(B_T_P_t$B, B_T_P_t$T, method = "pearson"))

  write(paste0(
    "correlation_wilcoxon: ", corr_w$estimate, ", ", corr_w$p.value, ", conf int: (", corr_w$conf.int[1], ", ", corr_w$conf.int[2], ")\n",
    "correlation_ttest: ", corr_t$estimate, ", ", corr_t$p.value, ", conf int: (", corr_t$conf.int[1], ", ", corr_t$conf.int[2], ")\n"
  ), paste0(plot_folder, "correladtion.txt")
  )
}

# x <- c(1, 2, 3, 4, 5)
# y <- x**2
# cor.test(x,y, method = "pearson")

var(B_T_P_t$B,B_T_P_t$T)/sd(B_T_P_t$B)/sd(B_T_P_t$T)