context("Testing summary method for PCR objects")

source("../sample_data.R")

init_study <- summary(beta_t_studied)
restudy <- summary(beta_t_restudied)
tested <- summary(beta_t_tested)
control <- summary(beta_t_no_prac)

test_that("Practice methods are correctly specified", {
  expect_equal(init_study$practice, c("C","C"))
  expect_equal(restudy$practice, c("S","C"))
  expect_equal(tested$practice, c("T","C"))
  expect_equal(control$practice, c("C","C"))
})
