#' @title forget
#'
#' @inheritParams study
#' @export
forget <- function(x, ...) {
  UseMethod("forget")
}

#' @describeIn forget Effects of interference on encoded features
#' @inheritParams study.PCRparams
#' @export
forget.PCR <- function(x, cue = 1) {
  x$activations[,,cue] <- x$PRforgetting(x$activations[,,cue])
  return(x)

}

