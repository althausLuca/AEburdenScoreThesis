library(gamlss)
library(tweedie)
source("R/trials/trial_loader.R")

trial_data <- load_shorter_trials()

links <- c("inverse", "log", "identity")

trial <- trial_data$trials[[1]]
non_zero_data <- trial[trial$Score > 0,]

model <- gamlss(Score ~ factor(Group), sigma.formula = ~1, family = GA(mu.link="log"), data = non_zero_data)
summary(model)
# Mu Coefficients:
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)     2.82180    0.14925  18.906   <2e-16 ***
# Grouptreatment -0.09512    0.18578  -0.512     0.61
model <- gamlss(Score ~ factor(Group), sigma.formula = ~1, family = GA(mu.link="identity"), data = non_zero_data)
summary(model)
# Mu link function:  identity
# Mu Coefficients:
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)      16.807      2.509   6.700 1.01e-09 ***
# Grouptreatment   -1.525      3.025  -0.504    0.615

trial <- trial_data$trials[[7]]
non_zero_data <- trial[trial$Score > 0,]

r <- abs(rnorm(length(non_zero_data[,1]), 1, 1))

model_gamma.1 <- glm(Score ~ factor(Group)+r, family = Gamma(link = "log"), data = non_zero_data, control = glm.control(epsilon = 1e-26, maxit = 1000))
summary(model_gamma.1)$coef[2,4] # p_value 0.673
fitted(model_gamma.1)[1] #16.80701
model_gamma.2 <- glm(Score ~ factor(Group)+r, family = Gamma(link = "identity"), data = non_zero_data,control = glm.control(epsilon = 1e-26, maxit = 1000))
summary(model_gamma.2)$coef[2,4] # p_value 0.673
fitted(model_gamma.2)[1] #16.80701
model_gamma.1$residuals[1]
model_gamma.2$residuals[1]

fitted(model_gamma.1)[1] #16.80701
fitted(model_gamma.2)[1] #16.80701
summary(model_gamma.1)$dispersion
summary(model_gamma.2)$dispersion



anova(model_gamma.1, test = "Rao")
anova(model_gamma.2, test = "Rao")


vcov(model_gamma.1)
vcov(model_gamma.2)

results <- vector("list", length(links))
for (mu.link in links) {
  p_values <- vector("list", length(trial_data$trials))
  robust_p_values <- vector("list", length(trial_data$trials))
  AIC_values <- vector("list", length(trial_data$trials))
  treatment_effect_values <- vector("list", length(trial_data$trials))
  for (trial_i in seq_along(trial_data$trials)) {
    if(trial_i > 2) { break }
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

names(results)
results$identity[[1]][[1]]
results$log[[1]][[1]]
r_summary <- summary(GA_model, robust = TRUE, control = gamlss.control(trace = FALSE))[2, ]
non_r_summary <- summary(GA_model, control = gamlss.control(trace = FALSE))[2, ]
r_summary
non_r_summary
#
# sum(abs(unlist(results$inverse$AIC_values) - unlist(results$identity$AIC_values)))
#
#
# results_w_group <- vector("list", length(links))
# for (mu.link in links) {
#   p_values <- vector("list", length(trial_data$trials))
#   robust_p_values <- vector("list", length(trial_data$trials))
#   AIC_values <- vector("list", length(trial_data$trials))
#   treatment_effect_values <- vector("list", length(trial_data$trials))
#   for (trial_i in seq_along(trial_data$trials)) {
#     trial <- trial_data$trials[[trial_i]]
#     non_zero_data <- trial[trial$Score > 0,]
#     gamlss_family <- do.call(GA, list(mu.link = mu.link))
#     GA_model <- gamlss(Score ~ Group, sigma.formula = ~Group, family = gamlss_family, data = non_zero_data)
#     GA_p_mu_treatment <- summary(GA_model)[2, 4]
#     GA_p_mu_treatment_robust <- summary(GA_model, robust = TRUE)[2, 4]
#     p_values[[trial_i]] <- GA_p_mu_treatment
#     robust_p_values[[trial_i]] <- GA_p_mu_treatment_robust
#     AIC_values[[trial_i]] <- AIC(GA_model)
#     treatment_effect_values[[trial_i]] <- summary(GA_model)[2, 1]
#   }
#   results_w_group[[mu.link]] <- list(p_values = p_values, robust_p_values = robust_p_values, AIC_values = AIC_values)
# }
#
# # withouth Group effect on sigma
# print("no group effect on sigma")
# for(link in links){
#     print(link)
#     cat("p_value " , mean(unlist(results[[link]]$p_values) < 0.05) , "\n")
#     cat("robust_p_value",mean(unlist(results[[link]]$robust_p_values) < 0.05), "\n")
# }
#
# # withGroup effect on sigma
# print("Group effect on sigma")
# for(link in links){
#   print(link)
#   cat("p_value " , mean(unlist(results_w_group[[link]]$p_values) < 0.05) , "\n")
#   cat("robust_p_value",mean(unlist(results_w_group[[link]]$robust_p_values) < 0.05), "\n")
# }
#
#
# pdf("../../../../AIC_differences_GA_longer_plot.pdf", width = 8, height = 6)
# AIC_diff  <- unlist(results$identity$AIC_values) - unlist(results_w_group$identity$AIC_values)
#
# # Create the boxplot
# boxplot(AIC_diff,
#         main = "Boxplot of AIC Differences Across Simulations",
#         ylab = "AIC Difference",
#         col = "lightblue",   # set color to light blue
#         border = "black",    # set border color
#         notch = TRUE)        # add notches to the boxplot for visual comparison of medians
#
# dev.off()
#
# min(AIC_diff)
# mean(AIC_diff)

