#'  PCR Cued Recall Simulation
#'
#' @param x A PCR model object
#' @param ... Additional arguments that may be passed to specific methods
#'
#' @export
cuedRecall <- function(x, cue = 1, ...) {
  UseMethod("cuedRecall")
}

cuedRecall.timed <- function(x, cue = 1, increment = TRUE, ...) {
  x$RT[,,cue] <- x$params$Tmin + (x$params$Tmax-x$params$Tmin) * exp(-x$params$lambda * abs(x$activations[,,cue]-x$thresholds))
  x$recalled[,,cue] <- (x$activations[,,cue] > x$thresholds) & (x$RT[,,cue] <= x$params$Tmax)
  x <- space_out(x, cue)
  nCor <- sum(x$recalled[,,cue])
  x <- PRlearning(x, only_recalled = TRUE, samples = nCor)
  x <- CRlearning(x, only_recalled = TRUE, samples = nCor)

}

cuedRecall.PCR <- function(x, cue = 1, increment  = TRUE, ...) {
  x$recalled[,,cue] <- (x$activations[,,cue] > x$thresholds)
  x <- space_out(x, cue)
  nCor <- sum(x$recalled[,,cue])
  x <- PRlearning(x, only_recalled = TRUE, samples = nCor)
  x <- CRlearning(x, only_recalled = TRUE, samples = nCor)

}

space_out <- function(x, cue = 1) {
  if ('space' %in% names(x$params) && x$params$space >= 0) {
    # recalled items stay the same with probability 1-space out
    x$recalled[,,cue][x$recalled[,,cue]] <- rbinom(sum(x$recalled[,,cue]), 1, 1-x$params$space)
  }
  return(x)
}

#' @title PCR Free Recall Simulation
#' @param x A PCR model object
#' @param cue A numeric value indicating which cue a test is simulated for
#' @param ... Additional arguments passed to specific methods
#'
#' @export
freeRecall <- function(x, cue = 1, ...) {
  UseMethod("freeRecall")
}

#' @export
freeRecall.timed <- function(x, cue = 1, ...) {

}

#' @importFrom dplyr first
#' @export
freeRecall.PCRbinomial <- function(x, cue = 1, ...) {

}

#' @importFrom dplyr first
#' @export
freeRecall.PCRbeta <- function(x, cue = 1, ...) {

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
