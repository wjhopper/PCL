library(PCL)
context("Testing Free Recall")

# Some constants to use
mem <-structure(c(56L, 55L, 61L, 51L, 57L, 51L, 59L, 50L, 51L, 56L,
                  50L, 53L, 54L, 57L, 58L),
                .Dim = c(3L, 5L))
thresh <- structure(c(56L, 46L, 46L, 50L, 55L, 52L, 43L, 52L, 56L, 49L,
                      51L, 48L, 47L, 51L, 54L),
                    .Dim = c(3L, 5L))
tmin = .5
tmax = 75
lambda = .5
time = 90

test_that("RT is a function of threshold distance", {
  result <- freeRecall(mem, thresh, Tmin=tmin,Tmax=tmax,lambda= lambda,Time=time)
  predRT <- tmin + (tmax-tmin)*exp(-lambda*abs(mem-thresh))
  expect_equal(predRT, result$RT)
})
