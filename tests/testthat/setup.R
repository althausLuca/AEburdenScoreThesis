library(testthat)
print(here::here())

setwd("../..")
source("R/Scenarios.R")
source("R/trials/trial_simulation.R")
source("R/methods/run_methods.R")
source("R/AdverseEvents/event_simulation.R")
source("R/AdverseEvents/AdverseEvents.R")

test_score.df <- data.frame(score = rexp(100), group = factor(c(rep("control", 50), rep("treatment", 50))))

AEs.treatment <- list(
  AE(3, 5, c(0.1, 0.1, 0.9)),
    AE(10, 15,  , c(0.1, 0.1, 0.9)),
    AE(20, 25, , c(0.1, 0.1, 0.9))
)
AEs.control <- list(
  AE(3, 5, c(0.1, 0.1, 0.9)),
  AE(10, 15, c(0.1, 0.1, 0.9)),
  AE(20, 25, c(0.1, 0.1, 0.9))
)

AEs.no_score <- list(
  AE(3, "10000000000000", c(0.1, 0.1, 0.9)),
  AE(0, 15,  , c(0.1, 0.1, 0.9))
)


