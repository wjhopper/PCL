#' @title Study
#' @description Effects of studying on un-encoded features
#'
#' @param x  An object used to select a method. The PCR package implements methods
#' for \code{timed} and \code{PCR} objects.
#' @param ... Additional arugments passed to other methods.
#'
#' @export
study <- function(x, ...) {
  UseMethod("study")
}

#' @describeIn study Effects of the initial study on memory
#' @param x A PCR model object
#' @param cue A scalar numeric indicating which cue to test on
#' @export
study.PCRparams <- function(x, nCues = 1, ...) {

  starting_point <- matrix(0L, nrow = x$nSim, ncol = x$nItems)

  mems <- replicate(nCues, x$PRlearning(matrix(0L, nrow = x$nSim, ncol = x$nItems),
                                        p = x$params$ER))
  `dimnames<-`(mems, list(NULL, NULL,  paste0("cue", 1:nCues)))
  thresh <- x$CRlearning(matrix(100L, nrow = x$nSim, ncol = x$nItems),
                         p = .5)

  object <- c(list(activations = mems, thresholds = thresh,
                   recalled = array(NA, dim = c(x$nSim, x$nItems, nCues)),
                   practice = setNames(rep(NA, nCues), paste0("Cue",1:nCues))),
              x)

  if (x$time > 0) {
    object$RT <- object$recalled
  }
  class(object) <- c("timed"[x$time > 0], paste0("PCR", x$distribution), "PCR")

  return(object)
}

#' @export
study.PCR<- function(x, cue = 1) {
  x$activations[,,cue] <- x$PRlearning(x$activations[,,cue], p = x$params$LR)
  x <- record_practice(x, "study", cue)
  return(x)
}

