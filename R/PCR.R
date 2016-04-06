#' Title
#'
#' @param params A named list of numerics to be used as PCR model parameters.
#' @param distribution A character vector naming the underlying distribution of
#' feature activation and thresholds. Can be either \code{"beta"} or \code{"binomial"}.
#' @param time A numeric scalar indicating the duration of the trial. If set to 0, then
#' retrieval latency is not simulated by PCR functions.
#' @param nFeatures A scalar numeric value giving the number of features each item
#' is assumed to have in memory.
#' @param nSims A scalar numeric value giving the number of times a memory process
#' should be simulated in the model.
#' @param nItems A scalar numeric value giving the number of items to be remembered.
#'
#' @return A PCRparams object
#'
#' @importFrom whoppeR betaParams
#' @export
#'
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

  x$PRlearning <- PRlearning_factory(x)
  x$CRlearning <- CRlearning_factory(x)

  class(x) <- "PCRparams"
  return(x)
}

#' @title updatePCRparams
#' @description  Update PCRparams object and method with new parameter values
#' @export
#' @examples
#' a <- initPCRparams(parameters = list(ER=.6, LR=.2, FR=.1, TR=.15, TV = .05),
#'                    distribution = "beta", nItems = 20, nSims = 1000, nFeatures = 100)
#' new_a <- updatePCRparams(a, new = list(ER = .1, LR = .75))
updatePCRparams <- function(x, new) {

  setdifference <- setdiff(names(new), names(x$params))
  if (length(setdifference) != 0) {
    stop(paste("Can't update model parameters: Parameters", setdifference, "do not exist in current parameter list"))
  }
  x$params[names(new)] <- new

  # Update the PR and CR learning methods to enclose the new parameter values
  x$PRlearning <- PRlearning_factory(x)
  x$CRlearning <- CRlearning_factory(x)

  return(x)
}

record_practice <- function(x, method, cue) {
  x$practice[paste0('Cue',cue)] <- method
  return(x)
}

PRlearning_factory <- function(x) {

  params <- x$params
  max <- x$nFeatures

  if (x$distribution == 'beta') {

    f <- function(activations, p = params$LR) {
      to_go <- floor(max - activations)

      # binomial mean = np, binomial variance = np(1-p)
      # bernoulli mean = p, bernoulli variance = p(1-p)
      activation_params <- betaParams(mean = p, sd = sqrt(p/to_go))
      learned  <- rbeta(n = length(activations),
                        shape1 = activation_params$a,
                        shape2 = activation_params$b) * to_go
      return(activations + learned)
    }

  } else {

    f <- function(activations, p = params$LR) {
      return(activations + rbinom(n = length(activations), size = max - activations, prob = p))
    }

  }

  return(f)
}

CRlearning_factory <- function(x) {

  params <- x$params
  max <- x$nFeatures

  if (x$distribution == 'beta') {

    f <- function(thresholds, p = params$TR) {
      known <- ceiling(thresholds)

      # binomial mean = np, binomial variance = np(1-p)
      # bernoulli mean = p, bernoulli variance = p(1-p)
      thresh_params <- betaParams(mean = p, sd = sqrt((p*(1-p))/known))
      lowered  <- rbeta(n = length(known),
                        shape1 = thresh_params$a,
                        shape2 = thresh_params$b) * known
      return(thresholds - lowered)
    }

  } else {

    f <- function(thresholds, p = params$TR) {
      return(thresholds - rbinom(n = length(thresholds), size = thresholds, prob = p))
    }

  }

  return(f)
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

  accuracy <- apply(x$recalled, 3, mean)
  RT <- apply(x$RT, 3, median)
  x$practice[is.na(x$practice)] <- "control"
  prac <- toupper(substr(x$practice,1,1))
  results <- data.frame(cue = 1:length(prac), practice = prac,
                        accuracy, RT,
                        row.names = NULL,
                        stringsAsFactors = FALSE)
}
