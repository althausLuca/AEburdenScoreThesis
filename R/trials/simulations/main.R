# File to analyzed single trials mostly on a gorup level e.g sucsesibility
# impact on the score of a single group
rm(list = ls())

source("R/AdverseEvents/AdverseEvents.R")
source("R/trials/trial_simulation.R")
source("R/Scenarios.R")


AEs.default <- list(AE(3, 60, MOSTLY_MILD)
  , AE(7, 60, MOSTLY_MODERATE)
  , AE(3, 20, MOSTLY_MILD)
)

AEs.3 <- list(AE(3, 28 * 2, MOSTLY_MILD)
  , AE(7, 28 * 2, MOSTLY_MODERATE)
  , AE(3, 3 * 2, MOSTLY_MILD)
)

AEs.increase <- list(AE(3, 50, MOSTLY_MILD)
  , AE(7, 50, MOSTLY_MODERATE)
  , AE(3, 25, MOSTLY_MILD)
)

AEs.3 <- list(AE(3, 100, MOSTLY_MILD)
  , AE(7, 100, MOSTLY_MODERATE)
  , AE(3, 3 * 10, MOSTLY_MILD)
)

theoretical_prob <- function(mean1,mean2,mean3){
    lambda1 <- 1 / mean1
    lambda2 <- 1 / mean2
    lambda3 <- 1 / mean3

    p_x1 <- 1-pexp(180, lambda1)
    p_x2 <- 1-pexp(180, lambda2)
    p_x3 <- 1-pexp(180, lambda3)

    joint_probability <- p_x1 * p_x2 * p_x3
    return(joint_probability)
}

AEs <- AEs.3
print(sapply(AEs, function(x) x$gap_time))


group_values <- simulate_group(AEs, size = 5000, susceptibility_parameter = list("constant"), max_time = 180)
print("constant")
var(group_values$scores)
mean(group_values$scores)
mean(group_values$scores==0)

group_values <- simulate_group(AEs, size = 1000, susceptibility_parameter = list("gamma", 100000000), max_time = 180)
print("k=100000000")
var(group_values$scores)
mean(group_values$scores)
mean(group_values$scores==0)

group_values <- simulate_group(AEs, size = 1000, susceptibility_parameter = list("gamma", 10), max_time = 180)
print("k=10")
var(group_values$scores)
mean(group_values$scores)
mean(group_values$scores==0)

group_values <- simulate_group(AEs, size = 1000, susceptibility_parameter = list("gamma", 1), max_time = 180)
print("k=1")
var(group_values$scores)
mean(group_values$scores)
mean(group_values$scores==0)

group_values <- simulate_group(AEs, size = 1000, susceptibility_parameter = list("gamma", 0.5), max_time = 180)
print("k=0.5")
var(group_values$scores)
mean(group_values$scores)
mean(group_values$scores==0)




