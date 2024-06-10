source("R/AdverseEvents/death_case_computations.R")
library(testthat)

simulated_AE.1 <- list(
  info = data.frame(
    Start = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    End = c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105),
    Duration = c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5),
    Severity = c("mild", "severe", "mild", "severe", "mild", "severe", "mild", "severe", "mild", "severe", "mild")
  ),
  max_time = 180
)
simulated_AE.2 <- list(
  info = data.frame(
    Start = c(0, 10, 170),
    End = c(5, 150, 179),
    Duration = c(5, 140, 9),
    Severity = c("mild", "severe", "mild")
  ),
  max_time = 180
)

simulated_AE.3 <- list(
  info = data.frame(
    Start = numeric(0),
    End = numeric(0),
    Duration = numeric(0),
    Severity = numeric(0)
  ),
  max_time = 180
)

test_that("No death eveents", {
  x <- expect_no_error(get_AE_time(simulated_AE.1))
  expect_equal(x, Inf)
})

test_that("Death eveents", {
  time_until_death <- expect_no_error(get_death_time(list(simulated_AE.2)))
  simulatedAEs <- expect_no_error(set_death(list(simulated_AE.1, simulated_AE.2)))
  expect_equivalent(time_until_death, TIME_UNTIL_DEATH[SEVERE] + 10,
                    info = paste0("time_until_death should be ", TIME_UNTIL_DEATH[SEVERE] + 10, " is ", time_until_death))
  expect_equivalent(tail(simulatedAEs[[1]]$info$Duration, n = 1), tail(simulatedAEs[[1]]$info$Duration, n = 1))
  expect_equivalent(tail(simulatedAEs[[2]]$info$End, n = 1), tail(simulatedAEs[[1]]$info$End, n = 1))
  expect_equivalent(tail(simulatedAEs[[2]]$info$Severity, n = 1), tail(simulatedAEs[[1]]$info$Severity, n = 1))
  expect_equivalent(tail(simulatedAEs[[2]]$info$Severity, n = 1), DEATH)
  expect_equivalent(tail(simulatedAEs[[2]]$info$End, n = 1), simulatedAEs[[2]]$max_time)
})

test_that("Death eveents one without events", {
  time_until_death <- expect_no_error(get_AE_time(simulated_AE.3))
  expect_equivalent(time_until_death, Inf,
                    info = paste0("time_until_death should be ", Inf, " is ", time_until_death))

  simulatedAEs_before <- list(simulated_AE.3)
  simulatedAEs <- expect_no_error(set_death(list(simulatedAEs_before)))
})