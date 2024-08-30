library(gamlss)
source("R/trials/trial_loader.R")


trial_data <- load_equal_trials()
trial <- trial_data$trials[[7]]


ZAGA_model <- gamlss(Score ~ Group, sigma.formula = ~1, nu.formula = ~Group, family = ZAGA(mu.link = "log"), data = trial)
{ sink("/dev/null") ; ZAGA_mu_treatment <-  summary(ZAGA_model)[2, ] ; sink();}

ZAGA_mu_treatment

non_zero_data <- trial[trial$Score > 0,]

GA_model <- gamlss(Score ~ Group, sigma.formula = ~1, family = ZAGA(mu.link = "log"), data = non_zero_data)
GA_mu_treatment.1 <- summary(GA_model, type="qr")[2, ]


GA_mu_treatment.2 <- summary(GA_model, type="vcov")[2, ]

GA_mu_treatment.1
GA_mu_treatment.2

glm_model <- glm(Score ~ Group, family = Gamma(link = "log"), data = non_zero_data)

glm_mu_treatment <- summary(glm_model)$coefficients[2, ]

ZAGA_mu_treatment
GA_mu_treatment

vcov(ZAGA_model)[2, 2]^0.5

vcov(GA_model, type = "se" )
#vcov(GA_model, type = "se" , robust = TRUE)


GA_model <- gamlss(Score ~ Group, sigma.formula = ~1, family = ZAGA(mu.link = "log" , nu.link = "logit"), data = non_zero_data)
vcov(GA_model, type = "se" ,  hessian.fun = "../../..")
vcov(GA_model, type = "se" ,  hessian.fun = "PB")
rvcov(GA_model, type = "se" )
rvcov(GA_model, type = "se", hessian.fun = "../../..")


rvcov(ZAGA_model, type = "se" )
fitted(ZAGA_model, "nu")
fitted(GA_model, "nu")

GA_model$call$family

ZAGA_model <- gamlss(Score ~ Group,  family = ZAGA(mu.link = "log" , nu.link = "logit"), sigma.formula = ~1, data = non_zero_data)
summary(ZAGA_model , robust = TRUE)

exp( -0.17497)*2





summary(GA_model, robust= TRUE)



vcov(GA_model, type = "se" )

gamlss:::vcov.gamlss(GA_model, type = "se", hessian.fun = "../../..", robust = TRUE)

summary(glm_model)$coefficients[,2]


NO_model <- gamlss(Score ~ Group, sigma.formula = ~1, family = NO(), data = non_zero_data)
vcov(NO_model, type = "se" )
vcov(NO_model, type = "se" , robust= TRUE)

NO_model$par

ZAGA_model <- gamlss(Score ~ Group, sigma.formula = ~1, family = ZAGA(mu.link = "log" , nu.link = "logit"), data = non_zero_data)
ZAGA_model$par
vcov(ZAGA_model, type = "se" )
vcov(ZAGA_model, type = "se" , robust= TRUE)

GA_model <- gamlss(Score ~ Group, sigma.formula = ~1, family = GA(), data = non_zero_data)
GA_model$par
vcov(GA_model, type = "se" )
vcov(GA_model, type = "se" , robust= TRUE)


vcov(ZAGA_model, type = "se" , robust= TRUE)

dldm <- function (y, mu, sigma , nu) ifelse(y == 0, 0, (y - mu)/((sigma^2) * (mu^2)))

family <- ZAGA(mu.link = "log")
family$dldm <- dldm
ZAGA_model <- gamlss(Score ~ Group, sigma.formula = ~1, family = family, data = trial)


summary(ZAGA_model,robust = TRUE , type = "qr" )


1+1
ZAGA_model$par <- ZAGA_model$par[1:2]




# ZAGA_model$par <- ZAGA_model$par[1:2]
summary(ZAGA_model )


fam <- ZAGA(mu.link = "log")
fam$dldm
