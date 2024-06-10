# Load the saved workspace
library(statmod)
library(tweedie)

source("R/trials/trial_simulation.R")

file <- "Scenario_3_k_1.5_l_3.5.csv"
trial_data <- load_trial_data(file)

var.powers <- c(seq(1.1,2,by=0.1))

tweedie_aics <- data.frame( power=numeric() , AIC=numeric() )

for(trial_i in seq_along(trial_data$trials)){
  trial <- trial_data$trials[[trial_i]]
  for(var.power in var.powers){

  tweedie_model.l1 <- glm(trial$Score ~ trial$Group, family =
                       tweedie(var.power = var.power, link.power = 1), control = glm.control(maxit = 100))
  tweedie_aics <- rbind(tweedie_aics , data.frame( power=var.power , AIC=AICtweedie(tweedie_model.l1)) )

  }
  if(trial_i==1){ plot(tweedie_aics , ylim=c(800,2400) , xlim=c(1.1,1.9), xlab="Tweedie Power")}

  else {lines(tweedie_aics)}
  print(AIC(lm(Score~Group,data= trial)))


  abline(h=AIC(lm(Score~Group,data= trial)),col="red")

  if(trial_i >80){break}
}

legend("topright", legend = c("Tweedie AIC","ANOVA AIC"),
       col = c("black", "red"), lty = c(1, 1), lwd = c(1,1), cex = 0.8)




