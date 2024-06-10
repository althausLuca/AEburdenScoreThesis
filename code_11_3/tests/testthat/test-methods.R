test_that("compilation without errors", {
  expect_no_error(run_qauntile_regression(test.df))
  expect_no_error(run_tweedie(test.df))
  expect_no_error(run_anova(test.df))
  expect_no_error(run_lm(test.df))
})

test_that("simulate methods and check output", {
  expect_true(all(c("model", "p_value", "estimate", "std_err") %in% names(  run_lm(test.df))))
    expect_true(all(c("model", "AIC", "p_value", "std_err", "t_value", "estimate", "link.power", "var.power") %in% names(  run_tweedie(test.df))))
    expect_true(all(c("model", "p_value", "estimate", "std_err") %in% names(  run_anova(test.df))))
    expect_true(all(c("model", "p_value", "estimate", "std_err") %in% names(  run_qauntile_regression(test.df))))
})