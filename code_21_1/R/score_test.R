
simulate_score_simple <- function(lambda1,lambda2,n.it=5000){
  scores <- c()
  for(i in 1:n.it){
    score <- 0
    time <- 0
    while(time < T){
      between_time <- rexp(1,lambda1)
      time <- time+between_time

      if(time < T){ #space for an event
        time_before_event <- time
        event_time <- rexp(1,lambda2)
        time <- time+event_time
        if(time < T){
          score <- score+event_time
        }else{
          score <- score+T-time_before_event
        }
      }
    }
    scores <- c(scores,score)
  }
  return(mean(scores))
}


simulate_score_weighted <- function(lambda1,lambda2,n.it=5000){
  scores <- c()
  for(i in 1:n.it){
    score <- 0
    time <- 0
    while(time < T){
      between_time <- rexp(1,lambda1)
      time <- time+between_time

      if(time < T){ #space for an event
        time_before_event <- time
        event_time <- rexp(1,lambda2)
        time <- time+event_time
        weight <- sample(c(1,2,3),1,TRUE,prob=c(0.8,0.15,0.05))
        if(time < T){
          score <- score+event_time*weight
        }else{
          score <- score+(T-time_before_event)*weight
        }
      }
    }
    scores <- c(scores,score)
  }
  return(mean(scores))
}



# test limit case where lambdas are equal and large -> score -> T/2
lambda1 <- 1000
lambda2 <- 1000
T <- 180

# T*lambda1

expected_value <-  T*lambda1/(lambda2+lambda1)-lambda1/(lambda2+lambda1)^2
expected_value <-  expected_value + lambda1*exp(-(lambda2+lambda1)*T)/((lambda1+lambda2)^2)
expected_value



# test limit case where lambdas are equal and large -> score -> 0
lambda1 <- 0.00001
lambda2 <- 0.00001
T <- 180

# T*lambda1

expected_value <-  T*lambda1/(lambda2+lambda1)-lambda1/(lambda2+lambda1)^2
expected_value <-  expected_value + lambda1*exp(-(lambda2+lambda1)*T)/((lambda1+lambda2)^2)
expected_value



# comparison with mean of simulations

lambda1 <- 1/20
lambda2 <- 1/7
T <- 180

# T*lambda1
expected_value <-  T*lambda1/(lambda2+lambda1)-lambda1/(lambda2+lambda1)^2
expected_value <-  expected_value + lambda1*exp(-(lambda2+lambda1)*T)/((lambda1+lambda2)^2)
expected_value

simulate_score_simple(lambda1,lambda2)



#test independent severity weights

lambda1 <- 1/20
lambda2 <- 1/7
T <- 180

# T*lambda1

expected_value <-  T*lambda1/(lambda2+lambda1)-lambda1/(lambda2+lambda1)^2
expected_value <-  expected_value + lambda1*exp(-(lambda2+lambda1)*T)/((lambda1+lambda2)^2)


expectedt_weight <- 0.8*1+0.15*2+0.05*3
expected_value <- expectedt_weight*expected_value

expected_value

simulate_score_weighted(lambda1,lambda2)




### susceptibility impact on mean score
lambda1 <- 1/20
lambda2 <- 1/7

score <- simulate_score_simple(lambda1,lambda2,n.it=5000)
score

n.it <- 10000
scores.mult <- c()
scores.div <- c()
subs <- c()
scores <- simulate_score_simple(lambda1,lambda2,n.it=n.it)
for(i in 1:n.it){
  sub <- sample(c(1/0.9,0.000000000000000001),1 ,prob = c(0.9,0.1))
  subs <- c(subs,sub)
  score.div <- simulate_score_simple(lambda1/sub,lambda2,n.it=1)
  scores.div <- c(scores.div,score.div)
  score.mult <- simulate_score_simple(lambda1*sub,lambda2,n.it=1)
  scores.mult <- c(scores.mult,score.mult)

}

mean(scores)
mean(subs)
mean(scores.div)
mean(scores.mult)



# using gamma
lambda1 <- 1/20
lambda2 <- 1/7
T <- 180
shape <- 2
scale <- 1/shape
n.it <- 10000
scores.mult <- c()
scores.div <- c()
subs <- c()
scores <- simulate_score_simple(lambda1,lambda2,n.it=n.it)
for(i in 1:n.it){
  sub <- rgamma(1, shape, scale=scale)
  subs <- c(subs,sub)
  score.div <- simulate_score_simple(lambda1/sub,lambda2,n.it=1)
  scores.div <- c(scores.div,score.div)
  score.mult <- simulate_score_simple(lambda1*sub,lambda2,n.it=1)
  scores.mult <- c(scores.mult,score.mult)

}

mean(scores)
mean(subs)
mean(scores.div)
mean(scores.mult)

rate_mult <- mean(scores)/mean(scores.mult)
rate_mult









