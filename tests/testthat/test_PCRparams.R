context("Testing PCRparams related functions")

source("../sample_data.R")
beta_timed <- initPCRparams(params = param_list, distribution = "beta", nItems = 20,
                            nSims = 1000, nFeatures = 100, time = 10)

binom_timed <- initPCRparams(params = param_list, distribution = "binomial",nItems = 20,
                             nSims = 1000, nFeatures = 100, time = 10)

beta_untimed <- initPCRparams(params = param_list, distribution = "beta", nItems = 20,
                              nSims = 1000, nFeatures = 100)

binom_untimed <- initPCRparams(params = param_list, distribution = "binomial",nItems = 20,
                               nSims = 1000, nFeatures = 100)

beta_t_studied <- study(beta_timed, nCues = 2)
binom_t_studied <- study(binom_timed, nCues = 2)
beta_studied <- study(beta_untimed, nCues = 2)
binom_studied <- study(binom_untimed, nCues = 2)

test_that("study.PCRparams returns an object of class 'PCR'", {

  expect_equal(class(beta_t_studied), c("timed","PCRbeta","PCR"))
  expect_equal(class(binom_t_studied), c("timed","PCRbinomial","PCR"))
  expect_equal(class(beta_studied), c("PCRbeta","PCR"))
  expect_equal(class(binom_studied), c("PCRbinomial","PCR"))
})
