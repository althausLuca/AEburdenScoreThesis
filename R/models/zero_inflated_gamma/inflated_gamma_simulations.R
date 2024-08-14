library(gamlss)
source("R/trials/trial_loader.R")
library(statmod)
library(Rfast2)

#https://stats.stackexchange.com/questions/608662/standard-errors-in-gamlss-vs-lmer

# Load trial data
trial_data <- load_equal_trials()

trial <- trial_data$trials[[1]]


trial_zero_one <- trial
trial_zero_one$Score <- ifelse(trial$Score > 0, 1, 0)
## logistic regression
logistic_glm <- glm(Score~Group,family=binomial,data=trial_zero_one)
summary(logistic_glm)

## gamma regression on non-zero values
non_zero_trial <- trial[trial$Score > 0,]
gamma_glm <- glm(Score~Group,family=Gamma(link = "log"),data=non_zero_trial)
summary(gamma_glm)$coefficients
phi <- summary(gamma_glm)$dispersion

shape <- 1/phi
mu.control <- exp(coef(gamma_glm)[1])
mu.treatment <- exp(coef(gamma_glm)[1] + coef(gamma_glm)[2])

scale.control <- mu.control / shape
scale.treatment <- mu.treatment / shape

x <- 1:200
plot(x, pgamma(x, shape = shape, scale = scale.control))
lines(x , ecdf(non_zero_trial$Score[non_zero_trial$Group =="control"])(x), col = "red")

plot(x, pgamma(x, shape = shape, scale = scale.treatment))
lines(x , ecdf(non_zero_trial$Score[non_zero_trial$Group=="treatment"])(x), col = "red")

#[1] 1.681992


gamlss <- gamlss(Score ~ Group, sigma.formula = ~Group, nu.formula = ~Group, family = ZAGA, data = trial)
summary(gamlss)

# Nu link function:  logit
# Nu Coefficients:
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)      0.4473     0.2050   2.182   0.0303 *
# Grouptreatment  -0.1245     0.2882  -0.432   0.6662
#
# Sigma link function:  log
# Sigma Coefficients:
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)     0.12862    0.09738   1.321    0.188
# Grouptreatment -0.29443    0.13870  -2.123    0.035 *

# Mu link function:  log
# Mu Coefficients:
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)      2.8218     0.1821  15.495   <2e-16 ***
# Grouptreatment  -0.3239     0.2242  -1.445     0.15


gamlss.2 <- gamlss(Score ~ Group, sigma.formula = ~1, nu.formula = ~Group, family = GA, data = non_zero_trial)
summary(gamlss.2)

shape <- 1/exp( 0.001113 )^2
1/shape

summary(gamma_glm)$dispersion


q <- 1

mu <- exp(2.8218)
sigma <- exp(0.12862)

shape <- 1/sigma^2
dispersion <- 1/shape
scale <- mu * sigma^2

dGA(0, mu = mu, sigma = sigma)
dgamma(0.1, shape = shape , scale = scale)


n_sim <- 5000

inflated_p_values <- rep(0, n_sim)
gamma_p_values <- rep(0, n_sim)
lm_p_values <- rep(0, n_sim)
lm_p_values.2 <- rep(0, n_sim)
tweetie_p_values <- rep(0, n_sim)

for (i in seq_along(trial_data$trials)) {
  trial <- trial_data$trials[[i]]
  if (i > n_sim) { break }

  trial$Score <- rgamma(200, shape = 100, rate = 100)

  # model <- gamlss(Score ~ Group, sigma.formula = ~Group, nu.formula = ~Group,
  #                 family = ZAGA, data = trial)
  #
  # #p_value of mu for the treatment effect
  # inflated_p_value <- summary(model)[2, 4]


  non_zero_trial <- trial #[trial$Score > 0,]
  non_zero_trial$Score <- ifelse(non_zero_trial$Score == 0, 1, non_zero_trial$Score)

  gamma_model <- glm(Score ~ Group, family = Gamma(link = "log"), data = non_zero_trial)
  non_inflated <- summary(gamma_model)$coefficients[2, 4]

  lm_model <- lm(Score ~ Group, data = non_zero_trial)
  lm_model.2 <- lm(Score ~ Group, data = trial)

  tweedie_model <- glm(Score ~ Group, family = tweedie(var.power = 1.5, link.power = 0), data = trial)

  # inflated_p_values[i] <- inflated_p_value
  gamma_p_values[i] <- non_inflated
  lm_p_values[i] <- summary(lm_model)$coefficients[2, 4]
  lm_p_values.2[i] <- summary(lm_model.2)$coefficients[2, 4]
  tweetie_p_values[i] <- summary(tweedie_model)$coefficients[2, 4]


}

# plot
# hist(inflated_p_values, main = "Inflated p-values", xlab = "p-value")
# hist(non_inflated_p_values, main = "Non-inflated p-values", xlab = "p-value")
# hist(lm_p_values, main = "LM p-values", xlab = "p-value")

mean(gamma_p_values < 0.05)
mean(lm_p_values < 0.05)
mean(lm_p_values.2 < 0.05)
mean(tweetie_p_values < 0.05)