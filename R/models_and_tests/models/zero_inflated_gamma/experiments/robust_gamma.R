library(gamlss)
source("../../../trials/trial_loader.R")

trial_data <- load_shorter_trials()

links <- c("inverse", "log", "identity")

results <- vector("list", length(links))
for (mu.link in links) {
  p_values <- vector("list", length(trial_data$trials))
  robust_p_values <- vector("list", length(trial_data$trials))
  AIC_values <- vector("list", length(trial_data$trials))
  treatment_effect_values <- vector("list", length(trial_data$trials))
  for (trial_i in seq_along(trial_data$trials)) {
    trial <- trial_data$trials[[trial_i]]
    non_zero_data <- trial[trial$Score > 0,]
    gamlss_family <- do.call(GA, list(mu.link = mu.link))
    GA_model <- gamlss(Score ~ Group, sigma.formula = ~1, family = gamlss_family, data = non_zero_data)
    GA_p_mu_treatment <- summary(GA_model)[2, 4]
    GA_p_mu_treatment_robust <- summary(GA_model, robust = TRUE)[2, 4]
    p_values[[trial_i]] <- GA_p_mu_treatment
    robust_p_values[[trial_i]] <- GA_p_mu_treatment_robust
    AIC_values[[trial_i]] <- AIC(GA_model)
    treatment_effect_values[[trial_i]] <- summary(GA_model)[2, 1]
  }
  results[[mu.link]] <- list(p_values = p_values, robust_p_values = robust_p_values, AIC_values = AIC_values)
}

r_summary <- summary(GA_model, robust = TRUE, control = gamlss.control(trace = FALSE))[2, ]
non_r_summary <- summary(GA_model, control = gamlss.control(trace = FALSE))[2, ]
r_summary
non_r_summary

sum(abs(unlist(results$inverse$AIC_values) - unlist(results$identity$AIC_values)))


results_w_group <- vector("list", length(links))
for (mu.link in links) {
  p_values <- vector("list", length(trial_data$trials))
  robust_p_values <- vector("list", length(trial_data$trials))
  AIC_values <- vector("list", length(trial_data$trials))
  treatment_effect_values <- vector("list", length(trial_data$trials))
  for (trial_i in seq_along(trial_data$trials)) {
    trial <- trial_data$trials[[trial_i]]
    non_zero_data <- trial[trial$Score > 0,]
    gamlss_family <- do.call(GA, list(mu.link = mu.link))
    GA_model <- gamlss(Score ~ Group, sigma.formula = ~Group, family = gamlss_family, data = non_zero_data)
    GA_p_mu_treatment <- summary(GA_model)[2, 4]
    GA_p_mu_treatment_robust <- summary(GA_model, robust = TRUE)[2, 4]
    p_values[[trial_i]] <- GA_p_mu_treatment
    robust_p_values[[trial_i]] <- GA_p_mu_treatment_robust
    AIC_values[[trial_i]] <- AIC(GA_model)
    treatment_effect_values[[trial_i]] <- summary(GA_model)[2, 1]
  }
  results_w_group[[mu.link]] <- list(p_values = p_values, robust_p_values = robust_p_values, AIC_values = AIC_values)
}

# withouth Group effect on sigma
print("no group effect on sigma")
for(link in links){
    print(link)
    cat("p_value " , mean(unlist(results[[link]]$p_values) < 0.05) , "\n")
    cat("robust_p_value",mean(unlist(results[[link]]$robust_p_values) < 0.05), "\n")
}

# withGroup effect on sigma
print("Group effect on sigma")
for(link in links){
  print(link)
  cat("p_value " , mean(unlist(results_w_group[[link]]$p_values) < 0.05) , "\n")
  cat("robust_p_value",mean(unlist(results_w_group[[link]]$robust_p_values) < 0.05), "\n")
}


pdf("../../../../AIC_differences_GA_longer_plot.pdf", width = 8, height = 6)
AIC_diff  <- unlist(results$identity$AIC_values) - unlist(results_w_group$identity$AIC_values)

# Create the boxplot
boxplot(AIC_diff,
        main = "Boxplot of AIC Differences Across Simulations",
        ylab = "AIC Difference",
        col = "lightblue",   # set color to light blue
        border = "black",    # set border color
        notch = TRUE)        # add notches to the boxplot for visual comparison of medians

dev.off()

min(AIC_diff)
mean(AIC_diff)