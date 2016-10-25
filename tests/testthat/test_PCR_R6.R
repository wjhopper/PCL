context("Testing the PCR R6 class")

x <- PCR$new(ER = .5, LR = .15, TR = .1, FR = .1,
             nSim = 1000, nItems = 15, nFeatures = 100,
             tests_per_cue = list(one = 1, two = 1))

test_that("Testing PCR Object Creation", {
  # Primary Retrieval Activations
  expect_is(x$PR_strengths, "list")
  expect_equal(length(x$PR_strengths), 2)
  expect_named(x$PR_strengths, c("one","two"))
  expect_true(all(x$PR_strengths[[1]] == 0L))
  expect_true(all(x$PR_strengths[[2]] == 0L))
  expect_true(all(x$PR_strengths[["one"]] == 0L))

  # Convergent Retrieval Thresholds
  expect_is(x$CR_thresholds, "matrix")
  expect_lte(max(x$CR_thresholds), 100)
  expect_gte(max(x$CR_thresholds), 0)
})

test_that("Test $study Method", {
  strengths <- x$PR_strengths[[1]]
  set.seed(919)
  x$study(cue = 1)
  set.seed(919)
  strengths <- strengths + rbinom(15000, size = 100 - strengths, .5)
  expect_equal(strengths, x$PR_strengths[[1]])
})

test_that("Testing $restudy Method", {
  strengths <- x$PR_strengths[[1]]
  set.seed(919)
  x$restudy(cue = 1)
  expect_lte(max(x$PR_strengths[[1]]), 100)
  expect_gte(min(x$PR_strengths[[1]]), 0)
  set.seed(919)
  strengths <- strengths + rbinom(15000, size = 100 - strengths, .15)
  expect_equal(strengths, x$PR_strengths[[1]])
})

test_that("Testing $cuedRecall Method", {
  strengths <- x$PR_strengths[[1]]
  thresholds <- x$CR_thresholds
  corrects <-  strengths > thresholds
  set.seed(919)
  x$cuedRecall(cue = 1)
  expect_lte(max(x$CR_thresholds), 100)
  expect_gte(min(x$CR_thresholds), 0)
  set.seed(919)
  updated_thresholds <- thresholds[corrects] - rbinom(sum(corrects),
                                                      thresholds[corrects],
                                                      .1)
  expect_equal(mean(corrects), mean(x$recalled[[1]][,,1]))
  expect_equal(x$CR_thresholds[x$recalled[[1]][,,1]], updated_thresholds)
  expect_equal(x$CR_thresholds[!x$recalled[[1]][,,1]], thresholds[!corrects])
})

