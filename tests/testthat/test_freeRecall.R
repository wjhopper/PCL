context("Testing Free Recall")

# Some constants to use
mem <-structure(c(56L, 55L, 61L, 51L, 99L, 51L, 59L, 50L, 51L, 56L,
                  50L, 53L, 54L, 57L, 58L),
                .Dim = c(3L, 5L))
thresh <- structure(c(56L, 46L, 46L, 50L, 98L, 02L, 43L, 52L, 56L, 49L,
                      51L, 48L, 47L, 51L, 54L),
                    .Dim = c(3L, 5L))
tmin = .5
tmax = 75
lambda = .5
time = 90
lr = .15
tr = .1
result <- freeRecall(mem, thresh, Tmin=tmin,Tmax=tmax,lambda= lambda,Time=time)

test_that("RT is a function of threshold distance", {
  predRT <- tmin + (tmax-tmin)*exp(-lambda*abs(mem-thresh))
  expect_equal(predRT, result$RT)
})

test_that("RTcor and Accuracy are consistent with each other", {
  expect_false(any(result$Acc[is.na(result$RTcor)]==TRUE))
  expect_true(all(result$Acc[!is.na(result$RTcor)]==TRUE))
  expect_false(anyNA(result$RTcor[result$Acc]))
  expect_true(all(is.na(result$RTcor[!result$Acc])))
})

test_that("RTcor is a cummulative sum of RT", {
  does_match <- matrix(NA,nrow(mem),ncol(mem))
  for (x in 1:nrow(mem)) {
#     mem_line <- mem[x,order[x,]]
#     thresh_line <- thresh[x,order[x,]]
    rt<- result$RT[x,result$order[x,]]
    rt_cor <- result$RTcor[x,result$order[x,]]
    acc <- result$Acc[x,result$order[x,]]
    does_match <- cumsum(rt_cor[!is.na(rt_cor)]) - cumsum(rt[acc])
    expect_true(all(does_match==0))
  }
})
