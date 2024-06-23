library(tweedie)
library(statmod)

source("R/trials/trial_simulation.R")

initial_vars <- ls()


output_file_name <- "shorter.Rdata"
file <- "Scenario_2_k_1.5_s_0.5.csv"
trial_data <- get_trial_data(file)

link_power <- 1

results <- data.frame(
  tweedie_xi = numeric(),
  tweedie_phi = numeric(),
    tweedie_mu = numeric(),
    tweedie_mu_treatment = numeric(),
    lm_mu = numeric(),
    lm_mu_treatment = numeric(),
    lm_sigma = numeric(),
    lm_log_mu = numeric(),
    lm_log_mu_treatment = numeric(),
    lm_log_sigma = numeric()
)

for(trial in trial_data$trials){
  profile_result <- tweedie.profile(trial$Score ~ trial$Group, link.power = link_power, fit.glm=F , xi.vec
                                    = seq(1.1, 2, by=0.1))
  optimal_xi <- profile_result$xi.max
  var.power <- optimal_xi
  glm_tweedie <- glm(Score ~ Group, data=trial, family = tweedie(var.power = var.power, link.power = 1))

  mu_tweedie <- coef(glm_tweedie)[1]
  mu_tweeide_treatment <-  sum(coef(glm_tweedie))

  phi_tweedie <- round(summary(glm_tweedie)$dispersion,2)

  # lm
  lm <- lm(Score ~ Group, data=trial)
  mu_lm <- coef(lm)[1]
  mu_lm_treatment <- sum(coef(lm))
  sigma_lm <- summary(lm)$sigma

  #log-anova
  delta <- 100
  trial$log_score <- log(trial$Score + delta)
  lm_log <- lm(log_score ~ Group, data=trial)
  mu_lm_log <- coef(lm_log)[1]
  mu_lm_log_treatment <- sum(coef(lm_log))
  sigma_lm_log <- summary(lm_log)$sigma

  results <- rbind(results, data.frame(
    tweedie_xi = optimal_xi,
    tweedie_phi = phi_tweedie,
    tweedie_mu = mu_tweedie,
    tweedie_mu_treatment = mu_tweeide_treatment,
    lm_mu = mu_lm,
    lm_mu_treatment = mu_lm_treatment,
    lm_sigma = sigma_lm,
    lm_log_mu = mu_lm_log,
    lm_log_mu_treatment = mu_lm_log_treatment,
    lm_log_sigma = sigma_lm_log
  ))
  print(results)
  save(file=output_file_name , list = ls()[!ls() %in%  initial_vars])
}
