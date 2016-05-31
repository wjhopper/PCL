#' PCR Cued Recall Simulation
#' @inheritParams study
#' @export
cuedRecall <- function(x, cue = 1, ...) {
  test_num <- sapply(x$recalled[cue], function(y) min(which(is.na(y[1,1,]))))
  UseMethod("cuedRecall")
}

#' @inheritParams study.PCR
#' @export
cuedRecall.timed <- function(x, cue = 1, ...) {

  x$RT[[cue]][,, test_num] <- RT(x, cue = cue)
  x$recalled[[cue]][,, test_num] <- (x$activations[,,cue] > x$thresholds) &
                                    (x$RT[[cue]][,, test_num] <= x$params$Tmax)
  x <- cuedRecall.default(x, cue = cue, test_num = test_num, ...)
  return(x)
}

#' @inheritParams study.PCR
#' @export
cuedRecall.PCR <- function(x, cue = 1, ...) {

  x$recalled[[cue]][,, test_num] <- (x$activations[,,cue] > x$thresholds)
  x <- cuedRecall.default(x, cue = cue, test_num = test_num, ...)
  return(x)
}

#' @inheritParams study.PCR
#' param increment A logical scalar indicating whether or not to update feature activation
#' and item thresholds based on the outcome of the test.
#' @export
cuedRecall.default <- function(x, cue = 1, test_num = 1, increment  = TRUE) {
  if ('space' %in% names(x$params) && x$params$space >= 0) {
    x <- space_out(x, cue, test_num = test_num)
  }

  if (increment) {
    r <- x$recalled[[cue]][,,test_num]
    feature_updates <- x$PRlearning(x$activations[,,cue][r])
    x$activations[,,cue][r] <- feature_updates
    threshold_updates <-  x$CRlearning(x$thresholds[r])
    x$thresholds[r] <- threshold_updates
    x <- record_practice(x, cue, "test")
  }

  return(x)
}

#' @export
thresh_change <- function(x) {
  # unneed represents the decrease in the number of features which must be activated in PR,
  # making it easier to recall the item.
  # Its value is a negative change, so it is added to the current thresholds values in
  # order to *decrease* the threshold.
  unneeded <- x$CRlearning(x$thresholds, p = x$params$TC, delta = TRUE)
  # needed represents increase in the number of features which need to be
  # activated in PR, making it more difficult to recall the item.
  # It is a positive change, so its value is added to the curent thresholds in
  # order to increase the threshold.
  needed <- x$PRlearning(x$thresholds, p = x$params$TC, delta = TRUE)
  x$thresholds <- x$thresholds + unneeded + needed
  return(x)
}

space_out <- function(x, cue = 1, test_num = 1) {
  # recalled items stay the same with probability 1-space out
  not_spaced <- as.logical(rbinom(sum(x$recalled[[cue]][,,test_num]), 1, 1-x$params$space))
  x$recalled[[cue]][,,test_num][x$recalled[[cue]][,,test_num]] <- not_spaced
  return(x)
}

#' @title PCR Free Recall Simulation
#' @inheritParams study
#' @export
freeRecall <- function(x, cue = 1, ...) {
  UseMethod("freeRecall")
}

#' @inheritParams study.PCR
#' @export
freeRecall.timed <- function(x, cue = 1, ...) {

}

#' @inheritParams study.PCR
#' @export
freeRecall.PCR <- function(x, cue = 1, ...) {

}
freeRecall <- function(mem, thresh, space=NULL,Tmin=NULL,Tmax=NULL,
                       Time=NULL,lambda=NULL) {
  if (is.null(time)) {
    recalled <- mem >= thresh
    serialOrder <- t(apply(mem,1, order, decreasing = TRUE))
    return(list(Acc=recalled, order=serialOrder))

  } else {

    # Preallocate return structures
    RT <- matrix(NA, nrow = nrow(mem), ncol = ncol(mem))
    RTcor <- matrix(NA, nrow = nrow(mem), ncol = ncol(mem))
    serialOrder <- matrix(NA, nrow = nrow(mem), ncol = ncol(mem))
    recalled <- matrix(FALSE, nrow = nrow(mem), ncol = ncol(mem))
    recoverable <- matrix(FALSE, nrow =nrow(mem), ncol = ncol(mem))
    for (i in 1:nrow(RT)) {

      # Find search order, and reverse indices
      ord <- order(mem[i,],decreasing=TRUE) # mem order
      reverseOrd <- order(ord) # sim order

      # Calculate RT and accuracy
      rt <- Tmin + (Tmax-Tmin)*exp(-lambda*abs(mem[i,]-thresh[i,])) # rt in sim order
      crt <- cumsum(rt[ord]) # crt in mem order
      rec <- (mem[i,]  >= thresh[i,])[ord] # rec in mem order
      acc <- crt < Time & rec # acc in mem order
      rt_cor <- c(first(crt[acc],default = numeric(0)), diff(crt[acc])) # rt_cor in mem order

      # Fill in the output structures
      # In simulation order, not search order!!!!
      RT[i,] <- rt
      recalled[i,] <- acc[reverseOrd]
      recoverable[i,] <- rec[reverseOrd]
      rt_cor_tmp <- rep(NA,length(rt))
      rt_cor_tmp[acc] <- rt_cor
      RTcor[i,] <- rt_cor_tmp[reverseOrd]
      serialOrder[i,] <- ord
    } # close for
  } # close if switch

  return(list(Acc=recalled,RT=RT, RTcor = RTcor,
              order=serialOrder,recoverable = recoverable))

} # Close freeRecall


RT <- function(x, cue = 1, ...) {
  x$params$Tmin + (x$params$Tmax-x$params$Tmin) * exp(-x$params$lambda * abs(x$activations[,,cue]-x$thresholds))
}
