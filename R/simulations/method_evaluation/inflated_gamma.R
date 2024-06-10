# Load required functions
source("R/trials/trial_simulation.R")
source("R/simulations/method_evaluation/density_methods.R")

#"Scenario_3_k_1.5_l_3.5.csv"
#Scenario_2_k_1.5_s_0.5.csv
file <- "Scenario_3_k_1.5_l_3.5.csv"

# Load trial data
trial_data <- load_trial_data(file)

trial_data_1 <- trial_data$trials[[3]]

#install.packages("gamlss")
library(gamlss)
mymodel <- gamlss(Score ~ Group, sigma.formula = ~ Group, nu.formula = ~ Group,
                  family=ZAGA ,data=trial_data_1)

summary(mymodel)


dist_from_model <- function(gamlss_model,  x =  seq(-10,600,by=0.1)){

  mu_coefficients <- coef(gamlss_model, what = "mu")
  sigma_coefficients <- coef(gamlss_model, what = "sigma")
  nu_coefficients <- coef(gamlss_model, what = "nu")

  logit_inv <- function(x){1/(1+exp(-x))}

  mu_control <- exp(mu_coefficients[1])
  sigma_control <- exp(sigma_coefficients[1])
  nu_control <- logit_inv(nu_coefficients[1])

  mu_treatment <- exp(sum(mu_coefficients))
  sigma_treatment <- exp(sum(sigma_coefficients))
  nu_treatment <- logit_inv(sum(nu_coefficients))

  p_control <- pZAGA(x, mu = mu_control, sigma = sigma_control, nu = nu_control)
  p_treatment <- pZAGA(x, mu = mu_treatment, sigma = sigma_treatment, nu = nu_treatment)

  return(list(x=x,control=p_control,treatment=p_treatment))

}


p_values_from_model <- function(gamlss_model){
  model_summary <- summary(gamlss_model)

  mu_p_val <- model_summary[2,4]
  sigma_p_val <-  model_summary[4,4]
  nu_p_pal <- model_summary[6,4]

  p_values <- c("mu"=  mu_p_val , "sigma" = sigma_p_val , nu = nu_p_pal)
  return(p_values)
}

p_values <- p_values_from_model(mymodel)
p_dist <-  dist_from_model(mymodel)



plot(-100,0, xlim = c(-10,600), ylim=c(0,1))
color <- rgb(0, 0, 1, alpha = 0.04)
color_legend <- rgb(0, 0, 1, alpha = 0.8)

n_it <- 100

x =  seq(-10,600,by=0.1)

distributions.c <- list()
distributions.t <- list()
for(i in seq_along(trial_data$trials)){

  if(i> n_it){ break }

  trial <- trial_data$trials[[i]]

  mymodel <- gamlss(Score ~ Group, sigma.formula = ~ Group, nu.formula = ~ Group,
                    family=ZAGA ,data=trial)

  dist <- dist_from_model(mymodel,x=x)
  distributions.c[[i]] <- dist$control
  distributions.t[[i]] <- dist$treatment
}

all_data <- trial_data$all_data()

# control plot
matplot(x, do.call(cbind, distributions.c), type = "l", lty = 1, col = color,
        xlim = c(-10, 200), ylim = c(0, 1),
        main = "",
        xlab = "Score", ylab = "P(x<=X)")


lines(ecdf(all_data[all_data$Group=="control",1]))

# treatment plot
matplot(x, do.call(cbind, distributions.t), type = "l", lty = 1, col = color,
        xlim = c(-10, 200), ylim = c(0, 1),
        main = "",
        xlab = "Score", ylab = "P(x<=X)")


lines(ecdf(all_data[all_data$Group=="treatment",1]))



## p_values analysis
p_value_list <- list()
for(i in seq_along(trial_data$trials)){

  if(i> 500){ break }

  trial <- trial_data$trials[[i]]

  mymodel <- gamlss(Score ~ Group, sigma.formula = ~ Group, nu.formula = ~ Group,
                    family=ZAGA ,data=trial)

  p_values <- p_values_from_model(mymodel)
  p_value_list[[i]]  <- p_values
}

p_mat <- do.call(cbind, p_value_list)
p_mat <- t(p_mat)
# p_values plot
matplot(p_mat, type = "l", lty = 1,
       ylim = c(0, 1),
        main = "",
        xlab = "", ylab = "P Value")
legend("topright", legend = colnames(p_mat), col = 1:ncol(p_mat), lty = 1, cex = 0.8)

colSums(p_mat <0.05)/length(p_mat[,1])
