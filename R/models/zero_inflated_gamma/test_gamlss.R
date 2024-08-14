source("tests/test_data.R")
source("R/models/fit_models.R")
source("R/models/models.R")


test_trial[6,1] <- 0
test_trial[7,1] <- 0
# test_trial[1,1] <- 9.9


model <- ZERO_INFLATED_GAMMA()
fit_model(model, test_trial)

modle_result <- gamlss(Score ~ Group, sigma.formula = ~Group, nu.formula = ~Group,
       family = ZAGA(mu.link = "log"), data = test_trial)
