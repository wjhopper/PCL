#' @title Forget
#' @description Effects of interference on encoded features
#'
#' @param x  An object used to select a method
#' @param ... Additional arugments passed to other methods.
#'
#' @export
forget <- function(x, cue = 1,...) {
  samples <- prod(dim(x$activations[,,cue]))
  UseMethod("forget")
}

#' @export
forget.PCRbinomial <- function(x, cue = 1) {

  # rbinom(number of repetitions, number of binomial trials, probability of success)
  forgot  <- matrix(rbinom(samples, x$activations[,,cue], x$params$FR),
                    nrow = x$nSim, ncol = x$nItems)
  x$activations[,,cue] <- x$activations[,,cue] - forgot
  return(x)

}

#' @export
forget.PCRbeta <- function(x, cue = 1) {

  known <- ceiling(x$activations[,,cue])

  # binomial mean = np, binomial variance = np(1-p)
  # bernoulli mean = p, bernoulli variance = p(1-p)
  activation_params <- betaParams(mean = x$params$FR,
                                  sd = sqrt(x$params$FR/known))
  forgot <- matrix(rbeta(samples, activation_params$a, activation_params$b) * known,
                   nrow = x$nSim, ncol = x$nItems)
  x$activations[,,cue] <- x$activations[,,cue] - forgot
  return(x)
}
