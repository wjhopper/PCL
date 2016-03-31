#' @title Study
#' @aliases PRlearning
#' @description Effects of studying on un-encoded features
#'
#' @param x  An object used to select a method
#' @param ... Additional arugments passed to other methods.
#'
#' @export
study <- function(x, cue = 1, ...) {
  UseMethod("study")
}

#' @export
#' @importFrom whoppeR betaParams
study.PCRparams <- function(x, nCues = 1, ...) {

  if (x$distribution == "beta") {

    # binomial mean = np, binomial variance = np(1-p)
    # bernoulli mean = p, bernoulli variance = p(1-p)
    activation_params <- betaParams(mean = x$params$ER, sd = sqrt(x$params$ER/x$nFeatures))
    mems <- replicate(nCues, matrix(rbeta(x$nSim*x$nItems, activation_params$a, activation_params$b) * x$nFeatures,
                                    nrow = x$nSim, ncol = x$nItems))
    mems <- setNames(mems,  paste0("cue", 1:nCues))
    thresh_params <- betaParams(mean = .5,sd = x$params$TV)
    thresh <- matrix(rbeta(x$nSim*x$nItems, thresh_params$a, thresh_params$b) * x$nFeatures,
                     nrow = x$nSim, ncol = x$nItems)

  } else {
    mems <- replicate(nCues, matrix(rbinom(x$nSim*x$nItems, x$nFeat, x$params$ER),
                                    nrow = x$nSim, ncol = x$nItems))
    # rbinom(number of repetitions, number of binomial trials, probability of success)
    thresh <- rbinom(x$nSim*x$nItems, x$nFeat, .5)
  }

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
study.PCRbinomial <- function(x, cue = 1) {

  samples <- prod(dim(x$activations[,,cue]))
  to_go <- x$nFeatures - x$activations[,,cue]

  # rbinom(number of repetitions, number of binomial trials, probability of success)
  learned  <- matrix(rbinom(samples, to_go, x$params$LR),
                     nrow = x$nSim, ncol = x$nItems)
  x$activations[,,cue] <- x$activations[,,cue] + learned
  return(x)

}

#' @export
study.PCRbeta <- function(x, cue = 1) {

  samples <- prod(dim(x$activations[,,cue]))
  to_go <- floor(x$nFeatures - x$activations[,,cue])

  # binomial mean = np, binomial variance = np(1-p)
  # bernoulli mean = p, bernoulli variance = p(1-p)
  activation_params <- betaParams(mean = x$params$LR,
                                  sd = sqrt(x$params$LR/to_go))
  learned  <- matrix(rbeta(samples, activation_params$a, activation_params$b) * to_go,
                     nrow = x$nSim, ncol = x$nItems)
  x$activations[,,cue] <- x$activations[,,cue] + learned
  return(x)
}
