# context("Testing RT")

# result <- freeRecall(mem, thresh, Tmin=tmin,Tmax=tmax,lambda= lambda,Time=time)
#
# test_that("RT is a function of threshold distance", {
#   predRT <- tmin + (tmax-tmin)*exp(-lambda*abs(mem-thresh))
#   expect_equal(predRT, result$RT)
# })
#
# test_that("RTcor and Accuracy are consistent with each other", {
#   expect_false(any(result$Acc[is.na(result$RTcor)]==TRUE))
#   expect_true(all(result$Acc[!is.na(result$RTcor)]==TRUE))
#   expect_false(anyNA(result$RTcor[result$Acc]))
#   expect_true(all(is.na(result$RTcor[!result$Acc])))
# })
#
# test_that("RTcor is a cummulative sum of RT", {
#   does_match <- matrix(NA,nrow(mem),ncol(mem))
#   for (x in 1:nrow(mem)) {
# #     mem_line <- mem[x,order[x,]]
# #     thresh_line <- thresh[x,order[x,]]
#     rt<- result$RT[x,result$order[x,]]
#     rt_cor <- result$RTcor[x,result$order[x,]]
#     acc <- result$Acc[x,result$order[x,]]
#     does_match <- cumsum(rt_cor[!is.na(rt_cor)]) - cumsum(rt[acc])
#     expect_true(all(does_match==0))
#   }
# })
