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
#' param_list <- list(ER=.6, LR=.15, FR=.1, TR=.1, TV = .05, Tmax = 75, Tmin = 0.5, lambda = .5)
#'
#' beta_timed <- initPCR(params = param_list, distribution = "beta", nItems = 20,
#'                            nSims = 1000, nFeatures = 100, time = 10)
#'
initPCR <- function(params, distribution = "beta", time = 0, nFeatures= 1000, nSims = 1000, nItems) {

  supplied <- as.list(match.call(expand.dots = TRUE))[-1]
  defaults <- formals()
  x <- c(supplied, defaults[setdiff(names(defaults), c(names(supplied), "..."))])
  unevaluated <- sapply(x, is.name)
  x[unevaluated] <- lapply(x[unevaluated], eval)

  x$PRlearning <- PRlearning_factory(x)
  x$CRlearning <- CRlearning_factory(x)
  x$PRforgetting <- PRforgetting_factory(x)

  class(x) <- "PCRparams"
  return(x)
}

#' @title updatePCRparams
#' @description  Update PCRparams object and method with new parameter values
#' @export
#' @examples
#' param_list <- list(ER=.6, LR=.15, FR=.1, TR=.1, TV = .05, Tmax = 75, Tmin = 0.5, lambda = .5)
#'
#' beta_timed <- initPCR(params = param_list, distribution = "beta", nItems = 20,
#'                            nSims = 1000, nFeatures = 100, time = 10)
#' new <- updatePCRparams(a, new = list(ER = .1, LR = .75))
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
  x$practice[[cue]] <- c(x$practice[[cue]], method)
  return(x)
}

PRlearning_factory <- function(x) {

  params <- x$params
  max <- x$nFeatures

  if (x$distribution == 'beta') {

    f <- function(activations, p = params$LR) {
      to_go <- ceiling(max - activations)

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

PRforgetting_factory <- function(x) {

  params <- x$params
  max <- x$nFeatures

  if (x$distribution == 'beta') {

    f <- function(activations, p = params$FR) {
      to_go <- ceiling(activations)

      # binomial mean = np, binomial variance = np(1-p)
      # bernoulli mean = p, bernoulli variance = p(1-p)
      activation_params <- betaParams(mean = p, sd = sqrt(p/to_go))
      forgot  <- rbeta(n = length(activations),
                       shape1 = activation_params$a,
                       shape2 = activation_params$b) * to_go
      return(activations - forgot)
    }

  } else {

    f <- function(activations, p = params$FR) {
      # rbinom(number of repetitions, number of binomial trials, probability of success)
      forgot <- rbinom(n = length(activations), size = activations, prob = p)
      return(activations - forgot)
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
#' beta_timed <- initPCR(params = param_list, distribution = "beta", nItems = 20,
#'                             nSims = 1000, nFeatures = 100, time = 10)
#'
#' init_study <- summary(beta_timed)
#' restudy <- summary(init_study)
#' summary(init_study)
#' summary(restudy)
#' restudy <- summary(beta_t_restudied)
summary.PCR <- function(x) {

  nCues <- dim(x$activations)[3]
  # x$practice[is.na(x$practice)] <- "control"
  x$practice <- lapply(x$practice, function(x) ifelse(is.null(x), "C", toupper(substr(x, 1, 1))))


  IV <- lapply(1:nCues, function(cue) {
    if (!is.na(x$recalled[cue])) {
      nTests <- dim(x$recalled[[cue]])[3]
      recalled <- apply(x$recalled[[cue]], 3, mean)
      RT <- apply(x$RT[[cue]], 3, median)
      data.frame(cue, practice = x$practice[[cue]],
                 test = 1:nTests, accuracy = recalled, RT,
                 row.names = NULL, stringsAsFactors = FALSE)
    }
  })

  return(do.call(rbind, IV))
}
