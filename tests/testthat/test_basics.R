context("Testing study, recall, and forgetting methods")

source("../sample_data.R")
min_val <- 0
max_val <- 100

test_that("study increases feature activation but doesn't touch threshold", {

  # beta
  expect_true(all(beta_t_restudied$activations[,,1] >= beta_t_studied$activations[,,1]))
  expect_equal(beta_t_restudied$thresholds, beta_t_studied$thresholds)

  # binomial
  expect_true(all(binom_t_restudied$activations[,,1] >= binom_t_studied$activations[,,1]))
  expect_equal(binom_t_restudied$thresholds, binom_t_studied$thresholds)
})


test_that("testing increases feature activations and lowers thresholds", {

  # beta
  expect_true(all(beta_t_tested$activations[,,1] >= beta_t_tested$activations[,,1]))
  expect_true(all(beta_t_tested$thresholds <= beta_t_tested$thresholds))

  # binomial
  expect_true(all(binom_t_tested$activations[,,1] >= binom_t_tested$activations[,,1]))
  expect_true(all(binom_t_tested$thresholds <= binom_t_tested$thresholds))
})

test_that("forgetting lowers feature activations but not thresholds", {

  # beta
  expect_true(all(beta_t_no_prac$activations[,,1] <= beta_t_studied$activations[,,1]))
  expect_equal(beta_t_no_prac$thresholds, beta_t_studied$thresholds)

  # binomial
  expect_true(all(binom_t_no_prac$activations[,,1] <= binom_t_studied$activations[,,1]))
  expect_equal(binom_t_no_prac$thresholds, binom_t_studied$thresholds)
})


test_that("study and test keep feature actvations between min and max", {

  # beta
  expect_true(all(beta_t_restudied$activations[,,1] <= max_val))
  expect_true(all(beta_t_restudied$activations[,,1] >= min_val))
  expect_true(all(beta_t_tested$thresholds <= max_val))
  expect_true(all(beta_t_tested$thresholds >= min_val))

  # binomial
  expect_true(all(binom_t_restudied$activations[,,1] <= max_val))
  expect_true(all(binom_t_restudied$activations[,,1] >= min_val))
  expect_true(all(binom_t_tested$activations[,,1] <= max_val))
  expect_true(all(binom_t_tested$activations[,,1] >= min_val))
  expect_true(all(binom_t_tested$thresholds <= max_val))
  expect_true(all(binom_t_tested$thresholds >= min_val))
})

#   expect_true(all(testStrengths$mem[result$Acc] >= mem[result$Acc]))
#   expect_true(all(testStrengths$thresh[result$Acc] <= thresh[result$Acc]))
#   expect_identical(testStrengths$mem[!result$Acc], mem[!result$Acc])
#   expect_identical(testStrengths$thresh[!result$Acc], thresh[!result$Acc])
#
#   expect_true(all(testStrengths$mem[result$Acc] >= mem_continuous[result$Acc]))
#   expect_true(all(testStrengths$thresh[result$Acc] <= thresh_continuous[result$Acc]))
#   expect_identical(testStrengths$mem[!result$Acc], mem_continuous[!result$Acc])
#   expect_identical(testStrengths$thresh[!result$Acc], thresh_continuous[!result$Acc])

