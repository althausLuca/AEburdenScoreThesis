# File to analyzed single trials mostly on a gorup level e.g sucsesibility
# impact on the score of a single group


source("R/AdverseEvents/AdverseEvents.R")
source("R/trials/trial_simulation.R")
source("R/Scenarios.R")


AEs <- list(AE(3, 28 , MOSTLY_MILD) ,
            AE(7, 28, MOSTLY_MODERATE),
            AE(3,3, MOSTLY_MILD))

group_values <- simulate_group(AEs, size=1000,  susceptibility_parameter = list("gamma", 0.1) , max_time=180)

scores.sorted <- group_values$scores[order(group_values$scores)]
susceptibility.sorted <- group_values$susceptibility[order(group_values$scores)]

plot(scores.sorted, susceptibility.sorted, xlab = "Score", ylab = "Susceptibility")

plot(scores.sorted, susceptibility.sorted, xlab = "Score", ylab = "Susceptibility" , ylim=c(0,150))

plot(scores.sorted, susceptibility.sorted, xlab = "Score", ylab = "Susceptibility" , ylim=c(0,15000))



group_values <- simulate_group(AEs, size=5000,  susceptibility_parameter = list("constant") , max_time=180)
max(1/group_values$susceptibility)
min(group_values$susceptibility)
max(group_values$scores)
sum(group_values$scores == 0)

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



AEs <- list(AE(3, 50 , MOSTLY_MILD) ,
            AE(7, 50, MOSTLY_MODERATE),
            AE(3,10, MOSTLY_MILD))



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
