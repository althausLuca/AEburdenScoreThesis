library(tweedie)
source("R/trials/trial_loader.R")
source("R/models_and_tests/models_and_tests.R")
source("R/evaluation/plot_functions/p_value_plot.R")

library(mgcv)


trial_data <- load_longer_trials()

scenario <- "longer_1_1.2_sub"
xi_ <- 1.2
link_power <- 1


# trial <- trial_data$trials[[1]]
# full_model <- glm(trial$Score ~ trial$Group, family = statmod::tweedie(var.power = xi_, link.power = link_power))
# GLM_LRT(full_model)


GLM_LRT <- function(full_model){

  #really bad ways to get xi and the link_power but I did not find elsewhere and dont want to evalute the call
  xi <-log(full_model$family$variance(exp(1))) # only works for tweedie
  link_power <- full_model$family$linkfun(1) # only works for 0 or 1

  if (is.null(link_power)) {
    link_power <- 0
  }

  dev <- deviance(full_model)
  dev.null <- full_model$null.deviance

  phi_model <- summary(full_model)$dispersion

  #phi from pearson residuals
  V_mu <- full_model$family$variance(fitted(full_model))
  X.squared <- sum((full_model$y - fitted(full_model))^2 / V_mu)
  phi_bar <- X.squared / (length(full_model$y) - 2)

  F_bar <- (dev.null - dev) / phi_bar
  F_model <- (dev.null - dev) / phi_model

  p_value_F_bar <- pf(F_bar, 1, length(full_model$y) - 2, lower.tail = FALSE)
  p_value_F_model <- pf(F_model, 1, length(full_model$y) - 2, lower.tail = FALSE)

  # p_anova_F <- anova(full_model, test = "F")$`Pr(>F)`[2]

  null_model <- glm(full_model$y~ 1, family = statmod::tweedie(var.power = xi, link.power = link_power))
  phi_null <- summary(null_model)$dispersion
  T.ll <- -2 * (logLiktweedie(null_model, dispersion = phi_bar) - logLiktweedie(full_model, dispersion = phi_bar))

  T.dev <- (full_model$null.deviance - deviance(full_model)) / phi_model
  deviance_p_value <- pchisq(abs(T.dev), df = 1, lower.tail = FALSE)
  ll_p_value <- pchisq(abs(T.ll), df = 1, lower.tail = FALSE)

  return(list(p_value_F_bar = p_value_F_bar,
              p_value_F_model = p_value_F_model,
              # p_anova_F = p_anova_F,
              ll_p_value = ll_p_value,
              deviance_p_value = deviance_p_value))
}

lrt_func <- function(trial, xi = xi_) {
  trial <- trial_sub_sampler(trial, 20)
  if (is.null(xi)) {
    profile_result.full <- tweedie.profile(trial$Score ~ trial$Group, link.power = link_power, xi.vec
      = seq(1.05, 2, by = 0.1), method = "interpolation") #interpolation is faster than inversion and yiels the same results
    # profile_result.null <- tweedie.profile(trial$Score ~ 1, link.power = link_power, xi.vec
    #   = seq(1.05, 2, by = 0.1), method = "interpolation")

    # ll_profile_p_value <- pchisq(-2 * (ll_profile_null - ll_profile_full), df = 1, lower.tail = FALSE)

    xi <- profile_result.full$xi.max
  }
  else {
    # ll_profile_p_value <- NA
  }
  xi <- as.numeric(xi)

  full_model <- glm(trial$Score ~ trial$Group, family = statmod::tweedie(var.power = xi, link.power = link_power))
  p_values <- GLM_LRT(full_model)
  # p_values$ll_profile_p_value <- ll_profile_p_value

  # our current results are based on the adjusted wald test
  model_fit <- fit_model.tweedie_glm_model(TWEEDIE_REGRESSION(xi = xi, link_power = link_power), trial)
  p_values$current <-  model_fit$p_value

  return(p_values)
}

results <- trial_data$apply_to_each(lrt_func, as.df = TRUE, limit = 1000)
head(results)
folder <- "R/experiments/tweedie/"
file_name <- paste0("tweedie_lrt_", scenario, xi_)

save(results, file = paste0(folder, file_name, ".RData"))

file_name <-"tweedie_lrt_longer_infer"
load(paste0(folder, file_name, ".RData"))
#(col, name , color)
col_color_map <- list(c("current", "Wald", "red"),
                      # c("deviance_p_value", "LRT deviance", "blue"),
                      c("ll_p_value", "LRT ", "black")
            # c("p_value_F_model", "LRT F model", "green"),
            # c("p_value_F_bar", "LRT using Deviance", "purple")
                      )

p_plot_handler <- p_value_plot_handler()
for (i in seq_along(col_color_map)) {
  col_name <- col_color_map[[i]][1]
  name <- col_color_map[[i]][2]
  color <- col_color_map[[i]][3]
  p_values <- unlist(results[[col_name]])
  p_plot_handler$add(p_values, name, color = color)
}
print(p_plot_handler$plot(infer_colors = FALSE))
p_plot_handler$save(paste0(file_name, ".pdf"), folder, infer_colors = FALSE)


# xi <- p <- 1.8
# trial <- trial_data$trials[[5]]
# full_model <- glm(trial$Score ~ trial$Group, family = statmod::tweedie(var.power = xi, link.power = link_power))
# mean(trial$Score[trial$Group == "treatment"])
# phi <- summary(full_model)$dispersion
#
# sum(ldTweedie(trial$Score,mu=fitted(full_model),p=xi,phi=phi))
# sum(ldTweedie(trial$Score,mu=fitted(full_model)-1,p=xi,phi=phi))
#
# sum(log(dtweedie(y = trial$Score, mu = fitted(full_model), phi = phi, power = xi )))
# sum(log(dtweedie(y = trial$Score, mu = fitted(full_model)-1, phi = phi, power = xi )))

#
# tweedie_LRT <- function(trial, xi , link_power){
#     null_model <- glm(trial$Score ~ 1, family = statmod::tweedie(var.power = xi, link.power = link_power))
#     full_model <- glm(trial$Score ~ trial$Group, family = statmod::tweedie(var.power = xi, link.power = link_power))
#
#     #using the models dispersion for phi
#     phi_full <- summary(full_model)$dispersion
#     phi_null <- summary(null_model)$dispersion
#
#     ll_full <- sum(log(dtweedie(y = trial$Score, mu = fitted(full_model), phi = phi_full, power = xi )))
#     ll_null <- sum(log(dtweedie(y = trial$Score, mu = fitted(null_model), phi = phi_null, power = xi )))
#
#     # check if it does the same as the implementation in the tweedie package
#     stopifnot(ll_full == logLiktweedie(full_model, dispersion = phi_full))
#     stopifnot(ll_null == logLiktweedie(null_model, dispersion =phi_null))
#
#     T <- -2 * (ll_null - ll_full)
#     p_value_ll <- pchisq(T, df = 1, lower.tail = FALSE)
#
#     #using defualt dispersion for phi  deviance(null_model)/200 (AICTweedie uses this)
#     ll_full_no_phi <- logLiktweedie(full_model)
#     ll_null_no_phi <- logLiktweedie(null_model)
#     T_no_phi <- -2 * (ll_null_no_phi - ll_full_no_phi)
#     p_value_ll_no_phi <- pchisq(T_no_phi, df = 1, lower.tail = FALSE)
#
#     #using mgcv::ldTweedie (https://stat.ethz.ch/R-manual/R-patched/library/mgcv/html/ldTweedie.html)
#     ll_full_mgcv <- sum(ldTweedie(trial$Score,mu=fitted(full_model),p=xi,phi=deviance(full_model)/200))
#     ll_null_mgcv <- sum(ldTweedie(trial$Score,mu=fitted(null_model),p=xi,phi=deviance(null_model)/200))
#     T_mgcv <- -2 * (ll_null_mgcv - ll_full_mgcv)
#     p_value_ll_mgcv <- pchisq(T_mgcv, df = 1, lower.tail = FALSE)
#
#     # using anova(... , test = "LRT")
#     p_value_anova_lrt <- anova(null_model, full_model, test = "LRT")$`Pr(>Chi)`[2] #https://stats.stackexchange.com/questions/155474/why-does-lrtest-not-match-anovatest-lrt
#
#     return(list(p_value_ll = p_value_ll, p_value_ll_no_phi = p_value_ll_no_phi, p_value_anova_lrt = p_value_anova_lrt, p_value_ll_mgcv = p_value_ll_mgcv))
# }


# link_powers <- c(0)
# xis <- c("infer")

