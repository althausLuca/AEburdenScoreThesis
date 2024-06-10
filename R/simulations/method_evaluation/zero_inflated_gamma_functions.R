get_permuted_trial <- function(trial){
  trial$Score <- sample(trial$Score,200, replace=F) #null distribution
  return(trial)
}

get_mean_estimate <- function(trial){

  trial$Group <- as.factor(trial$Group)
  trial$Group <- relevel(trial$Group, ref = "control")

  capture.output({
    model <- gamlss(Score ~ Group, sigma.formula = ~ Group, nu.formula = ~ Group,
                    family=ZAGA ,data=trial)

    logit_inv <- function(x){1/(1+exp(-x))}
    model_summary <- summary(model)

  })



  mu_est.c <- model_summary[1,1]
  mu_est.t <- mu_est.c +  model_summary[2,1]

  nu_est.c <- model_summary[5,1]
  nu_est.t <- nu_est.c + model_summary[6,1]

  mu_est.c <- exp(mu_est.c)
  mu_est.t <- exp(mu_est.t)

  nu_est.c <- logit_inv(nu_est.c)
  nu_est.t <- logit_inv(nu_est.t)

  mean_est.c <- (1-nu_est.c)*mu_est.c
  mean_est.t <- (1-nu_est.t)*mu_est.t

  print("control")
  print(mean_est.c)
  print(mean(trial$Score[trial$Group=="control"]))

  print("treatment")
  print(mean_est.t)
  print(mean(trial$Score[trial$Group=="treatment"]))

  #estimate 0 distribution for mean_est.c-mean_est.t
  return(c(mean_est.c,mean_est.t))
}
