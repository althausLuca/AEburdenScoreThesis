# File to analyzed single trials mostly on a gorup level e.g sucsesibility
# impact on the score of a single group
rm(list = ls())


source("R/AdverseEvents/AdverseEvents.R")
source("R/trials/trial_simulation.R")
source("R/Scenarios.R")


Constant <- c(1,0,0)

AEs <- list(AE(3, 300 , Constant)
            , AE(7, 300, Constant)
            ,AE(3, 150, Constant)
            )
duartion_shape <- 10

group_values <- simulate_group(AEs, size=5000,  susceptibility_parameter = list("gamma",1) , max_time=180)
simulation_config <- list(duartion_shape = 10)
#group_values$scores <- ceiling(group_values$scores)

scores.sorted <- group_values$scores[order(group_values$scores)]

susceptibility.sorted <- group_values$susceptibility[order(group_values$scores)]
score.constant <- mean(scores.sorted)



plot(scores.sorted^(1/2), log(susceptibility.sorted), xlab = "Score", ylab = "Susceptibility Log", main="gamma-k 2.5")


max(1/group_values$susceptibility)
min(group_values$susceptibility)
max(group_values$scores)
mean(group_values$scores)
sum(group_values$scores == 0)

hist(scores.sorted[scores.sorted>0], breaks=100)
hist(group_values$n_events , breaks=100)
hist(group_values$n_events[group_values$n_events > 1] , breaks=100)

scores_one_event <- group_values$scores[group_values$n_events==1]
max_events <- max(group_values$n_events)
hist(scores_one_event , breaks=500)
sort(scores_one_event)
max(scores_one_event)

max(scores.sorted)
group_values$scores[group_values$n_events==max_events]
max_i_events <- which(group_values$n_events==max_events)

group_values$susceptibility[max_i_events]

sort(group_values$scores[group_values$scores>0])


hist(rgamma(10000, shape=10 , scale=3/10) , breaks=1000)

for(theta in c(0.01, 0.1, 0.5, 1, 10 )){
  group_values <- simulate_group(AEs, size=5000,  susceptibility_parameter = list("gamma", theta) , max_time=180)
  print("theta")
  print(theta)
  # print(max(1/group_values$susceptibility))
  # print(min(group_values$susceptibility))
  # print(min(group_values$scores))
  print("zero count")
  print(sum(group_values$scores == 0))
  print("mean")
  print(mean(group_values$scores))
  print("max score")
  print(max(group_values$scores))
}



AEs <- list(AE(3, 1500 , MOSTLY_MODERATE) ,
            AE(7, 1500, MOSTLY_MILD),
            AE(3, 1000, MOSTLY_MILD))

par(mfrow=c(1,2))

for(theta in c(1.5)){
  group_values <- simulate_group(AEs, size=10000,  susceptibility_parameter = list("gamma", theta) , max_time=180)
  print("theta")
  print(theta)
  # print(max(1/group_values$susceptibility))
  # print(min(group_values$susceptibility))
  # print(min(group_values$scores))
  print("zero count")
  print(paste0(sum(group_values$scores == 0)/100,"%"))
  print("mean")
  print(mean(group_values$scores))
  print("max score")
  print(max(group_values$scores))
  #hist(group_values$scores,breaks=1000)
  hist(group_values$scores[group_values$scores>0],breaks=1000 ,
       main=paste("histogram of scores, #zeros:",sum(group_values$scores==0)))
  hist(group_values$n_events[group_values$n_events>0],breaks=100 , main=paste("histogram of zero truncated event counts"))
}
sort(group_values$scores,decreasing =TRUE)[1:10]



mean(group_values$scores[group_values$scores>0])
median(group_values$scores[group_values$scores>0])
length(group_values$scores[group_values$scores>200])
length(group_values$scores[group_values$scores>50])
sort(group_values$scores,decreasing =TRUE)[1:20]

AEs.treatment <- list(AE(3, 500 , MOSTLY_MILD) ,
            AE(10, 500, MOSTLY_MILD),
            AE(3, 1000, MOSTLY_MODERATE))

for(theta in c(1.5)){
  group_values <- simulate_group(AEs.treatment, size=5000,  susceptibility_parameter = list("gamma", theta) , max_time=180)
  print("theta")
  print(theta)
  # print(max(1/group_values$susceptibility))
  # print(min(group_values$susceptibility))
  # print(min(group_values$scores))
  print("zero count")
  print(paste0(sum(group_values$scores == 0)/50,"%"))
  print("mean")
  print(mean(group_values$scores))
  print("max score")
  print(max(group_values$scores))
  #hist(group_values$scores,breaks=1000)
  hist(group_values$scores[group_values$scores>0],breaks=1000 )
}
mean(group_values$scores[group_values$scores>0])
median(group_values$scores[group_values$scores>0])
length(group_values$scores[group_values$scores>200])
length(group_values$scores[group_values$scores>50])


AEs <- list(AE(3, 100,MOSTLY_MILD) ,
            AE(3, 100,MOSTLY_MILD),
            AE(7, 100, MOSTLY_MODERATE),
            AE(7, 100, MOSTLY_MODERATE),
            AE(3, 30, MOSTLY_MILD),
            AE(3, 30, MOSTLY_MILD))


for(theta in c(0.01, 0.1, 0.5, 1, 10 )){
  group_values <- simulate_group(AEs, size=5000,  susceptibility_parameter = list("gamma", theta) , max_time=180)
  print("theta")
  print(theta)
  # print(max(1/group_values$susceptibility))
  # print(min(group_values$susceptibility))
  # print(min(group_values$scores))
  print("zero count")
  print(sum(group_values$scores == 0))
  print("mean")
  print(mean(group_values$scores))
  print("max score")
  print(max(group_values$scores))
}

#plot scores and susceptibility values for the group (sorted by score)
scores.sorted <- group_values$scores[order(group_values$scores)]
susceptibility.sorted <- group_values$susceptibility[order(group_values$scores)]

plot(scores.sorted, susceptibility.sorted, xlab = "Score", ylab = "Susceptibility", main = "Susceptibility vs Score")

source("R/trials/trial_analysis.R")
hist_plot(group_values$score,zero_count = TRUE , mean = TRUE)
hist_plot(group_values$susceptibility,zero_count = TRUE , mean = TRUE)
