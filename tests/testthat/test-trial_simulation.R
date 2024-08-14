# test_that("compilation without errors",{
#   expect_no_error(simulate_trial(AEs.treatment, AEs.control, 180, 2))
#   expect_no_error(simulate_trial(AEs.treatment, AEs.control, 180, 2))
# })
#
# test_that("simulate_trial test",{
#   simulated_trial <- simulate_trial(AEs.treatment, AEs.control, 180, 2, death = TRUE)
#   expect_named(simulated_trial$treatment[[1]],c("score","group"))
# })


#test_that("compilation without errors",{
#  expect_no_error(simulate_scores_from_scenario(TESTSCENARIO, n_sim=10))
#  loaded_scores <- expect_no_error(load_scores(TESTSCENARIO))
#  expect_equal(length(loaded_scores), 10)
#})
