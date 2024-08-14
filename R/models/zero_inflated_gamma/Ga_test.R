library(gamlss)
source("R/trials/trial_loader.R")

trial_data <- load_equal_trials()
trial <- trial_data$trials[[7]]

non_zero_data <- trial[trial$Score > 0,]

GA_model <- gamlss(Score ~ Group, sigma.formula = ~1, family = GA(mu.link = "log"), data = non_zero_data)
p_values <- summary(GA_model, robust = TRUE)[2, ]
p_values


GA_model <- gamlss(Score ~ Group, sigma.formula = ~1, family = GA(mu.link = "log"), data = trial[trial$Score > 0,])
p_values.2 <- summary(GA_model, robust = TRUE)[2, ]

p_values
p_values.2



f <- function(x){
  print(match.call())
}

f()
f(THIS_SHIT_DOES_NOT_EVENT_EXIST)
