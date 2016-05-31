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
study.PCRparams <- function(x, nCues = 1, tests_per_cue = 1, ...) {

  if (length(tests_per_cue) == 1) {
    tests_per_cue <- rep(tests_per_cue, times = nCues)
  }

  create_recalled_array <- function(cue) {
    if (tests_per_cue[cue] > 0) {
      element <- array(dim = c(x$nSim, x$nItems, tests_per_cue[cue]))
    } else {
      element <- NA
    }
    return(element)
  }

  mems <- replicate(nCues, x$PRlearning(matrix(0L, nrow = x$nSim, ncol = x$nItems),
                                        p = x$params$ER))
  `dimnames<-`(mems, list(NULL, NULL,  paste0("cue", 1:nCues)))
  thresh <- x$CRlearning(matrix(100L, nrow = x$nSim, ncol = x$nItems),
                         p = .5)
  recalled <- setNames(lapply(1:nCues, create_recalled_array),
                       paste0("Cue",1:nCues))

  object <- c(list(activations = mems, thresholds = thresh, recalled = recalled), x)
  object <- record_practice(object, cue = 1:nCues, method = "study")
  if (x$time > 0) {
    object$RT <- object$recalled
  }
  class(object) <- c("timed"[x$time > 0], paste0("PCR", x$distribution), "PCR")

  return(object)
}

#' @describeIn study Effects restudying on feature activation memory
#' @inheritParams study.PCRparams
#' @export
study.PCR<- function(x, cue = 1, ...) {
  x$activations[,,cue] <- x$PRlearning(x$activations[,,cue], p = x$params$LR)
  x <- record_practice(x, cue, "study")
  return(x)
}

