context("Testing PCRparams related functions")

source("../sample_data.R")

test_that("study.PCRparams returns an object of class 'PCR'", {

  expect_equal(class(beta_t_studied), c("timed","PCRbeta","PCR"))
  expect_equal(class(binom_t_studied), c("timed","PCRbinomial","PCR"))
  expect_equal(class(beta_studied), c("PCRbeta","PCR"))
  expect_equal(class(binom_studied), c("PCRbinomial","PCR"))
})
