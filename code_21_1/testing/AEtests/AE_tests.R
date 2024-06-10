## Test simulation of AE
source("R/AdverseEvents/AdverseEvents.R")
source("R/AdverseEvents/event_simulation.R")
source("../../R/AdverseEvents/Vizualisation.R")

library(testthat)

test_that("compilation without errors",{
  expect_no_error(AE(time_to_first_event = 10, event_duration = 1, time_between_events = 10))
  ae <- AE(time_to_first_event = 10, event_duration = 1, time_between_events = 10)
  expect_no_error(simulate_event(ae, max_time = 180))
})

test_that("simulate_event simulates events correctly", {
  ae <- AE(time_to_first_event = 10, event_duration = 1, time_between_events = 10)
  simulated_ae <- simulate_event(ae, max_time = 180)
  expect_true(is.list(simulated_ae))
  expect_true("info" %in% names(simulated_ae))
  expect_equal(length(simulated_ae$info$Start), simulated_ae$n_events)
  expect_true(simulated_ae$info$Start[1] >= 0)
  expect_true(simulated_ae$score > 0, paste0("score should be greater than 0, is", simulated_ae$score))
})


test_that("Test empty AE", {

  ae <- AE(time_to_first_event = 100000000, event_duration = 10, time_between_events = 10)

  simulated_ae <- simulate_event(ae, max_time = 180)
  expect_true("info" %in% names(simulated_ae))
  expect_equal(simulated_ae$score, 0, info = paste0("score should be  0, is", simulated_ae$score))

  ae <- AE(time_to_first_event = 1, event_duration = 0, time_between_events = 10)
  simulated_ae <- simulate_event(ae, max_time = 180)
  expect_equal(simulated_ae$score, 0, info = paste0("score should be  0, is", simulated_ae$score))

})


test_that("simulate_events simulates multiple events correctly", {
  ae_list <- list(
    AE(time_to_first_event = 10, event_duration = 1, time_between_events = 10),
    AE(time_to_first_event = 8, event_duration = 2, time_between_events = 12)
  )

  simulated_ae_list <- simulate_events(ae_list, max_time = 180)
  expect_true(is.list(simulated_ae_list))
  expect_true(simulated_ae_list$score >= 0)
  pdf(file = NULL)

  expect_no_error(plot_AEs(simulated_ae_list$AEs,FALSE))
  expect_no_error(plot_AEs(simulated_ae_list$AEs,TRUE))
  dev.off()
})


test_that("simulate_events simulates multiple events with no event", {
  ae_list <- list(
    AE(time_to_first_event = 10000000, event_duration = 1, time_between_events = 10),
    AE(time_to_first_event = 8, event_duration = 0, time_between_events = 12)
  )

  simulated_ae_list <- simulate_events(ae_list, max_time = 180)
  expect_true(is.list(simulated_ae_list))
  expect_true(simulated_ae_list$score == 0)
  pdf(file = NULL)

  expect_no_error(plot_AEs(simulated_ae_list$AEs,FALSE))
  expect_no_error(plot_AEs(simulated_ae_list$AEs,TRUE))
  dev.off()
})


