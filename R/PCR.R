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
#' param_list <- list(ER=.6, LR=.15, FR=.1, TR=.1, TV = .05,
#'                    Tmax = 75, Tmin = 0.5, lambda = .5)
#' beta_timed <- initPCR(params = param_list, distribution = "beta", time = 10,
#'                       nFeatures = 100, nSims = 1000, nItems = 20)
#' str(beta_timed, give.attr=FALSE)
initPCR <- function(params, distribution = "beta", time = 0,
                    nFeatures= 1000, nSims = 1000, nItems) {

  supplied <- as.list(match.call(expand.dots = TRUE))[-1]
  defaults <- formals()
  x <- c(supplied, defaults[setdiff(names(defaults), c(names(supplied), "..."))])
  unevaluated <- sapply(x, is.name)
  x[unevaluated] <- lapply(x[unevaluated], eval)

  # The history data frame is the record of which cues were practiced,
  # how they were practiced, and in which order
  x$history = data.frame(cue = NULL, trial_type = NULL, stringsAsFactors = FALSE)

  # Add the learning/forgetting functions to the object as closures
  x <- c(x, learning_factory(x))
  class(x) <- "PCRparams"
  return(x)
}

#' @title update.PCRparams
#' @description Update a PCRparams object and methods with new parameter values
#' @export
#' @examples
#' param_list <- list(ER=.6, LR=.15, FR=.1, TR=.1, TV = .05,
#'                    Tmax = 75, Tmin = 0.5, lambda = .5)
#' beta_timed <- initPCR(params = param_list, distribution = "beta", time = 10,
#'                       nFeatures = 100, nSims = 1000, nItems = 20)
#' new <- update(beta_timed, new = list(ER = .1, LR = .75))
#' print(new$params)
update.PCRparams <- function(x, new) {

  setdifference <- setdiff(names(new), names(x$params))
  if (length(setdifference) != 0) {
    stop(paste("Can't update model parameters: Parameters", setdifference, "do not exist in current parameter list"))
  }
  x$params[names(new)] <- new

  # Update the learning and forgetting function to enclose the new parameter values
  learning_env <- environment(x$PRlearning)
  learning_env$params <- x$params

  return(x)
}

record_practice <- function(x, cue, method) {
  x$history <- rbind(x$history,
                     data.frame(cue = cue, trial_type = method, stringsAsFactors = FALSE))
  return(x)
}

learning_factory <- function(x) {
  params <- x$params
  max <- x$nFeatures

  if (x$distribution == 'beta') {
    # binomial mean = np, binomial variance = np(1-p)
    # bernoulli mean = p, bernoulli variance = p(1-p)

    ## Beta Primary Retrieval Learning function
    PRlearning <- function(activations, p = params$LR, delta = FALSE) {
      to_go <- ceiling(max - activations)

      activation_params <- betaParams(mean = p, sd = sqrt(p/to_go))
      learned  <- rbeta(n = length(activations),
                        shape1 = activation_params$a,
                        shape2 = activation_params$b) * to_go
      if (delta) {
        return(learned)
      } else {
        return(activations + learned)
      }
    }

    ## Beta Primary Retrieval Forgetting function
    PRforgetting <- function(activations, p = params$FR, delta = FALSE) {
      to_go <- ceiling(activations)
      activation_params <- betaParams(mean = p, sd = sqrt(p/to_go))
      forgot  <- rbeta(n = length(activations),
                       shape1 = activation_params$a,
                       shape2 = activation_params$b) * to_go
      if (delta) {
        return(-forgot)
      } else {
        return(activations - forgot)
      }
    }

    ## Beta Convergent Retrieval Learning Function
    CRlearning <- function(thresholds, p = params$TR, delta = FALSE) {
      known <- ceiling(thresholds)
      # binomial mean = np, binomial variance = np(1-p)
      # bernoulli mean = p, bernoulli variance = p(1-p)
      thresh_params <- betaParams(mean = p, sd = sqrt((p*(1-p))/known))
      reduction <- rbeta(n = length(known),
                         shape1 = thresh_params$a,
                         shape2 = thresh_params$b) * known
      if (delta) {
        return(-reduction)
      } else {
        return(thresholds - reduction)
      }
    }

  } else {
    ## Binomial Primary Retrieval Learning function
    PRlearning <- function(activations, p = params$LR, delta = FALSE) {
      learned <- rbinom(n = length(activations), size = max - activations, prob = p)
      if (delta) {
        return(learned)
      } else {
        return(activations + learned)
      }
    }

    ## Binomial Primary Retrieval Forgetting function
    PRforgetting <- function(activations, p = params$FR, delta = FALSE) {
      # rbinom(number of repetitions, number of binomial trials, probability of success)
      forgot <- rbinom(n = length(activations), size = activations, prob = p)
      if (delta) {
        return(-forgot)
      } else {
        return(activations - forgot)
      }
    }

    ## Binomial Convergen Retrieval Learning Function
    CRlearning <- function(thresholds, p = params$TR, delta = FALSE) {
      reduction <- rbinom(n = length(thresholds), size = thresholds, prob = p)
      if (delta) {
        return(-reduction)
      } else {
        return(thresholds - reduction)
      }
    }
  }

  return(list("PRlearning" = PRlearning, "PRforgetting" = PRforgetting,
              "CRlearning" = CRlearning))
}



#' Summarize a PCR model condition
#'
#' @param x A PCR model object
#'
#' @return A data frame
#'
#' @importFrom dplyr tbl_df
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
  IV <- lapply(1:nCues, cue_summary, x)
  return(do.call(rbind, IV))
}

cue_summary <- function(cue, x) {
  p <- which(x$history$cue == cue)
  if (length(p) == 1) {
    x$history$trial_type[p] <- "none"
  } else {
    x$history <- x$history[-p[1], ]
  }
  x$history <-`rownames<-`(x$history, NULL)

  if (!all(is.na(x$recalled[[cue]]))) {
    nTests <- dim(x$recalled[[cue]])[3]
    recalled <- apply(x$recalled[[cue]], 3, as.vector)
    recalled <- lapply(seq_len(ncol(recalled)), function(i) recalled[,i])
    rawRTs <- apply(x$RT[[cue]], 3, as.vector)
    rawRTs <- lapply(seq_len(ncol(rawRTs)), function(i) rawRTs[,i])
    correct_RT <- Map(`[`, rawRTs, recalled)
    incorrect_RT <- Map(`[`, rawRTs, lapply(recalled, `!`))
    accuracy <- as.vector(sapply(recalled, function(x) {m <- mean(x); c(m, 1-mean(x))}))
    medianRT <- as.vector(matrix(c(sapply(correct_RT, median),
                                   sapply(incorrect_RT, median)),
                                 byrow = TRUE, ncol = 2))
    rawRTs <- c(correct_RT, incorrect_RT)
    IV <- data.frame(setNames(x$history[x$history$cue==cue, ], c("cue","practice")),
                     test = rep(1:nTests, each = length(accuracy)/nTests),
                     accuracy =rep(c(1,0), times = nTests),
                     proportion = accuracy,
                     medianRT = medianRT,
                     stringsAsFactors = FALSE,
                     row.names = NULL)

    if (length(rawRTs) > 2) {
      IV$RTs <- rawRTs[order(c(seq_along(correct_RT), seq_along(incorrect_RT)))]
    } else {
      IV$RTs <- rawRTs
    }

  } else {
    IV <- data.frame(setNames(x$history[x$history$cue==cue, ], c("cue","practice")),
                     test=NA,  accuracy=NA, proportion = NA, medianRT = NA, RTs = NA,
                     stringsAsFactors = FALSE, row.names = NULL)
  }

 return(tbl_df(IV))

}
