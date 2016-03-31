context("Testing study, recall, and forgetting methods")

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

test_that("study increases feature activation but doesn't touch threshold", {

  beta_t_restudied <- study(beta_t_studied, cue = 1)
  expect_true(all(beta_t_restudied$activations[,,1] >= beta_t_studied$activations[,,1]))
  expect_equal(beta_t_restudied$thresholds, beta_t_studied$thresholds)

  binom_t_restudied <- study(binom_t_studied, cue = 1)
  expect_true(all(binom_t_restudied$activations[,,1] >= binom_t_studied$activations[,,1]))
  expect_equal(binom_t_restudied$thresholds, binom_t_studied$thresholds)
})

test_that("study keeps feature actvations between min and max", {

  beta_t_restudied <- study(beta_t_studied, cue = 1)
  expect_true(all(beta_t_restudied$activations[,,1] <= max_val))
  expect_true(all(beta_t_restudied$activations[,,1] >= min_val))

  binom_t_restudied <- study(binom_t_studied, cue = 1)
  expect_true(all(binom_t_restudied$activations[,,1] <= max_val))
  expect_true(all(binom_t_restudied$activations[,,1] >= min_val))
})

test_that("testing increases feature activations and lowers thresholds", {

  beta_t_tested <- cuedRecall(beta_t_studied, cue = 1)
  expect_true(all(beta_t_tested$activations[,,1] >= beta_t_tested$activations[,,1]))
  expect_true(all(beta_t_tested$thresholds <= beta_t_tested$thresholds))

  binom_t_tested <- cuedRecall(binom_t_studied, cue = 1)
  expect_true(all(binom_t_tested$activations[,,1] >= binom_t_tested$activations[,,1]))
  expect_true(all(binom_t_tested$thresholds <= binom_t_tested$thresholds))
})

test_that("forgetting lowers feature activations but not thresholds", {

  beta_t_no_prac <- forget(beta_t_studied, cue = 1)
  expect_true(all(beta_t_no_prac$activations[,,1] <= beta_t_studied$activations[,,1]))
  expect_equal(beta_t_no_prac$thresholds, beta_t_studied$thresholds)

  binom_t_no_prac <- forget(binom_t_studied, cue = 1)
  expect_true(all(binom_t_no_prac$activations[,,1] <= binom_t_studied$activations[,,1]))
  expect_equal(binom_t_no_prac$thresholds, binom_t_studied$thresholds)
})

#   expect_true(all(testStrengths$mem <= max))
#   expect_true(all(testStrengths$mem >= min))
#   expect_true(all(testStrengths$thresh <= max))
#   expect_true(all(testStrengths$thresh >= min))
#   expect_true(all(restudyStrengths >= mem))
#   expect_true(all(testStrengths$mem[result$Acc] >= mem[result$Acc]))
#   expect_true(all(testStrengths$thresh[result$Acc] <= thresh[result$Acc]))
#   expect_identical(testStrengths$mem[!result$Acc], mem[!result$Acc])
#   expect_identical(testStrengths$thresh[!result$Acc], thresh[!result$Acc])
#
# })
#
# test_that("(Beta) All strengths between the min and max features", {
#
#   expect_true(all(testStrengths$mem <= max))
#   expect_true(all(testStrengths$mem >= min))
#   expect_true(all(testStrengths$thresh <= max))
#   # The following test will fail stochastically on item #13...
#   expect_true(all(testStrengths$thresh >= min))
#   expect_true(all(restudyStrengths >= mem_continuous))
#   expect_true(all(testStrengths$mem[result$Acc] >= mem_continuous[result$Acc]))
#   expect_true(all(testStrengths$thresh[result$Acc] <= thresh_continuous[result$Acc]))
#   expect_identical(testStrengths$mem[!result$Acc], mem_continuous[!result$Acc])
#   expect_identical(testStrengths$thresh[!result$Acc], thresh_continuous[!result$Acc])
# })
#

