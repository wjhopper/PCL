
#' Create a PCRparams object to control be used by PCR functions like \code{\link{study}}.
#'
#'
#' @param params A named list of numerics to be used as PCR model parameters.
#' @param distribution A character vector naming the underlying distribution of
#' feature activation and thresholds. Can be either \code{"beta"} or \code{"binomial"}.
#' @param nFeatures A scalar numeric value giving the number of features each item
#' is assumed to have in memory.
#' @param nItems A scalar numeric value giving the number of items to be remembered.
#' @param nSim A scalar numeric value giving the number of times a memory process
#' should be simulated in the model.
#' @param time A numeric scalar indicating the duration of the trial. If set to 0, then
#' retrieval latency is not simulated by PCR functions.
#'
#' @export
#' @examples
#' a <- initPCRparams(params = list(ER=.6, LR=.2, FR=.1, TR=.15, TV = .05),
#'                    distribution = "beta", nItems = 20, nSims = 1000,
#'                    nFeatures = 100, time = 10)
initPCRparams <- function(params, distribution = "beta", time = 0, nFeatures= 1000, nSims = 1000, nItems) {

  supplied <- as.list(match.call(expand.dots = TRUE))[-1]
  defaults <- formals()
  x <- c(supplied, defaults[setdiff(names(defaults), c(names(supplied), "..."))])
  unevaluated <- sapply(x, is.name)
  x[unevaluated] <- lapply(x[unevaluated], eval)
  class(x) <- "PCRparams"
  return(x)
}

#' @export
#' @examples
#' a <- initPCRparams(parameters = list(ER=.6, LR=.2, FR=.1, TR=.15, TV = .05),
#'                    distribution = "beta", nItems = 20, nSims = 1000, nFeatures = 100)
#' new_a <- updatePCRparas(a, new = list(ER = .1, LR = .75))
updatePCRparams <- function(x, new) {
  setdifference <- setdiff(names(new), names(x$params))
  if (length(setdifference) != 0) {
    stop(paste("Can't update model parameters: Parameters", setdifference, "do not exist in current parameter list"))
  }
  x$params[names(new)] <- new
  return(x)
}


#' @title PR Learning
#' @description Strengthening of Cue --> Target associations on un-encoded features
#'
#' @param x  An object used to select a method
#' @param ... Additional arugments passed to other methods.
#'
#' @export
PRlearning<- function(x, ...) {
  UseMethod("PRlearning")
}

#' @export
PRlearning.PCRbinomial<- function(x, cue = 1, only_recalled = FALSE, samples = length(x$activations[,,cue]), ...) {
  if (only_recalled) {
    x$activations[,,cue][x$recalled[,,cue]] <- x$activations[,,cue][x$recalled[,,cue]] +
      rbinom(samples, x$nFeatures - x$activations[,,cue][x$recalled[,,cue]], x$params$LR)
  } else {
    x$activations[,,cue] <- x$activations[,,cue] + rbinom(samples, x$nFeatures - x$activations[,,cue], x$params$LR)
  }
  return(x)
}

#' @export
PRlearning.PCRbeta<- function(x, cue = 1, only_recalled = FALSE, samples = length(x$activations[,,cue]),  ...) {
  if (only_recalled) {
    to_go <- floor(x$nFeatures - x$activations[,,cue][x$recalled[,,cue]])
    activation_params <- betaParams(mean = x$params$LR,
                                    sd = sqrt(x$params$LR/to_go))
    learned  <- rbeta(samples, activation_params$a, activation_params$b) * to_go
    x$activations[,,cue][x$recalled[,,cue]] <- x$activations[,,cue][x$recalled[,,cue]] + learned

  } else {
    to_go <- floor(x$nFeatures - x$activations[,,cue])
    activation_params <- betaParams(mean = x$params$LR, sd = sqrt(x$params$LR/to_go))
    learned  <- rbeta(samples, activation_params$a, activation_params$b) * to_go
    x$activations[,,cue] <- x$activations[,,cue] + learned
  }
  return(x)
}


#' @title CR Learning
#' @description Strengthening of Target feature --> Target feature associations
#'
#' @param x  An object used to select a method
#' @param ... Additional arugments passed to other methods.
#'
#' @export
CRlearning<- function(x, ...) {
  UseMethod("CRlearning")
}

#' @export
CRlearning.PCRbinomial<- function(x, cue = 1, only_recalled = FALSE, samples = length(x$activations[,,cue]), ...) {
  if (only_recalled) {
    x$threshold[x$recalled[,,cue]] <- x$threshold[x$recalled[,,cue]] - rbinom(samples, x$threshold[x$recalled[,,cue]], x$params$TR)
  } else {
    x$threshold <- x$threshold - rbinom(samples, x$threshold, x$params$TR)
  }
  return(x)
}

#' @export
CRlearning.PCRbeta<- function(x, cue = 1, only_recalled = FALSE, samples = length(x$activations[,,cue]),  ...) {
  if (only_recalled) {
    known <- ceiling(x$thresholds[x$recalled[,,cue]])
    thresh_params <- betaParams(mean = x$params$TR, sd = sqrt(x$params$TR/known))
    lowered  <- rbeta(samples, thresh_params$a, thresh_params$b) * known
    x$thresholds[x$recalled[,,cue]] <- x$thresholds[x$recalled[,,cue]] - lowered

  } else {
    known <- ceiling(x$thresholds)
    thresh_params <- betaParams(mean = x$params$TR, sd = sqrt(x$params$TR/known))
    lowered <- rbeta(samples, thresh_params$a, thresh_params$b) * known
    x$thresholds <- x$thresholds - lowered
  }
  return(x)
}

#' Summarize a PCR model condition
#'
#' @param x A PCR model object
#'
#' @return A data frame
#' @export
#'
#' @examples
#' param_list <- list(ER=.6, LR=.15, FR=.1, TR=.1, TV = .05,
#'                    Tmax = 75, Tmin = 0.5, lambda = .5)
#' beta_timed <- initPCRparams(params = param_list, distribution = "beta", nItems = 20,
#'                             nSims = 1000, nFeatures = 100, time = 10)
#'
#' init_study <- summary(beta_timed)
#' restudy <- summary(init_study)
#' summary(init_study)
#' summary(restudy)
#' restudy <- summary(beta_t_restudied)
summary.PCR <- function(x) {

  accuracy <- apply(x$activations, 3, function(y) mean(y > x$thresholds))
  RT <- apply(x$RT, 3, median)
  x$practice[is.na(x$practice)] <- "control"
  prac <- toupper(substr(x$practice,1,1))
  results <- data.frame(cue = 1:length(prac), practice = prac,
                        accuracy, RT,
                        row.names = NULL,
                        stringsAsFactors = FALSE)
}

practice_method <- function(method, cue) {
  function(x) {
    x$practice[paste0('Cue',cue)] <- method
    return(x)
  }
}
