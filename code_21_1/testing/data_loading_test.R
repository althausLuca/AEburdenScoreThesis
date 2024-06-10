source("R/Scenarios.R")

library(testthat)

test_that("load scenario data, checks for clean data", {
  for(scenario in SCENARIOS){
    expect_no_error( data <- load_data(scenario))
    scores.1 <- data$scores[[1]]
    expect_s3_class( scores.1,"data.frame")
    expect_equal(length(scores.1[,1]) , scenario$observations * 2, info = paste0("length of scores should be ", scenario$observations * 2, " is ", length(scores)))
  }

})


test_that("load scenario data, checks for clean data", {
  score_df <- data.frame( scores = rep(100,100),groups= c(rep("control",50, rep("treatment",60))))
  for(scenario in SCENARIOS){
    expect_no_error( data <- load_data(scenario))
    scores.1 <- data$scores[[1]]
    expect_s3_class( scores.1,"data.frame")
    expect_equal(length(scores.1[,1]) , scenario$observations * 2, info = paste0("length of scores should be ", scenario$observations * 2, " is ", length(scores)))
  }
})