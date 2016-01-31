#' @title Study
#' @description Effects of studying on un-encoded features
#' @param ... Arugments passed to approriate functions based on type-checking. See details.
#' @details test will attempt to type-check the mem matrix and call the
#' appropriate function (study_beta for doubles,study_binomial for integers).
#' @export
#' @import whoppeR
#' @name study
study <- function(...) {
  if (typeof(list(...)$mem) == "double") {
    return(study_beta(...))
  } else if (typeof(list(...)$mem) == "integer") {
    return(study_binomial(...))
  } else {
    stop("unable to determine data type in matrix.
         Call study_beta or study_binomial directly")
  }
}


#' @title Test
#' @description Effects of Testing on un-encoded features and thresholds
#' @param ... Arugments passed to approriate functions based on type-checking. See details.
#' @details \code{test} is a conveince function, and will attempt to type-check
#' the code{mem} matrix to call the appropriate function (test_beta for doubles,
#' test_binomial for integers).
#'
#' \code{test_beta} and \code{test_binomial} can (and should) be called directly though,
#' especially when fitting models and computational time matters. See Functions section
#' for more details.
#' @import whoppeR
#' @export
#' @name test
test <- function(...) {
  if (typeof(list(...)$mem) == "double") {
    return(test_beta(...))
  } else if (typeof(list(...)$mem) == "integer") {
    return(test_binomial(...))
  } else {
    stop("unable to determine data type in matrix.
         Call test_beta or test_binomial directly")
  }
}

#' @describeIn study study_beta is for continous data
#' @param mem Matrix of memory strengths.
#' @param nFeatures Number of features the memory for each item has
#' @param LR Learning Rate. Gives the probability of learning a new features
#'  through practice
#' @param FR Forgetting Rate. Gives the probability of forgetting a feature
#'
#' @return Matrix of memory strengths updated after restudying
#' @export
study_beta <- function(mem, nFeatures, LR, FR = NULL) {

  mxn <- prod(dim(mem))
  mem[nFeatures - mem  < 1] <- nFeatures-1
  toGo <- nFeatures - mem
  binomVAR <- toGo*LR*(1-LR)
  binomM <- toGo*LR
  beta_pars <- betaParams(mean = binomM/toGo, sd = sqrt(binomVAR/(toGo^2)))
  mem <- mem + (rbeta(mxn, beta_pars$a, beta_pars$b) * toGo)

  if (!is.null(FR) && FR != 0) {
    mem[mem < 1] <- 1
    binomVAR <-  mem*FR*(1-FR)
    binomM <-  mem*FR
    beta_pars <- betaParams(mean = binomM/mem, sd = sqrt(binomVAR/(mem^2)))
    mem <- mem - (rbeta(mxn, beta_pars$a, beta_pars$b) * mem)
  }

  return(mem)
}

#' @describeIn test \code{test_beta} is designed to be used with a matrix of
#' continous feature and threhsold values (represented as doubles) drawn from
#' a beta distribution
#' @inheritParams study_beta
#' @param thresh Matrix of memory thresholds. Threshold is the number of active features
#'  needed to support pattern completetion
#' @param acc Logical matrix of performance for previous test
#' @param TR Threshold Reduction rate. Describes the probability of removing the need
#'  for a remembering a particular feature following successfull recall
#'
#' @return List of two matrices. mem is a matrix of memory strengths following succcesful
#'  recall. thresh is a matrix of feature thresholds following succesful recall.
#'
#' @export
test_beta <- function(mem, nFeatures, thresh, acc, LR, TR, FR=NULL) {

#   strengths <- mem #copy encoded features. Strengths will be updated and returned, not mem
#   theta  <- thresh #copy current thresholds. theta will be updated and returned, not thresh
  mxn <- prod(dim(mem))
  nCor = sum(acc)

  # memory feature updating
  mem[nFeatures - mem < 1] <- nFeatures-1
  toGo <-nFeatures - mem
  binomVAR <- toGo*LR*(1-LR)
  binomM <- toGo*LR
  beta_pars <- betaParams(mean = binomM/toGo, sd = sqrt(binomVAR/(toGo^2)))
  mem[acc] <- mem[acc] + (rbeta(nCor, beta_pars$a, beta_pars$b) * toGo[acc])

  # threshold updating
  thresh[thresh < 1] <- 1
  binomVAR <- thresh*TR*(1-TR)
  binomM <-  thresh*TR
  beta_pars <- betaParams(mean = binomM/thresh, sd = sqrt(binomVAR/thresh^2))
  thresh[acc] <- thresh[acc] - (rbeta(nCor, beta_pars$a[acc], beta_pars$b[acc])
                               * thresh[acc])

  if (!is.null(FR) && FR != 0) {
    mem[mem < 1] <- 1
    binomVAR <-  mem*FR*(1-FR)
    binomM <-  mem*FR
    beta_pars <- betaParams(mean = binomM/mem, sd = sqrt(binomVAR/(mem^2)))
    mem <- mem - (rbeta(mxn, beta_pars$a, beta_pars$b) * mem)
  }

  return(list(mem = mem,thresh = thresh))
}


#' @describeIn study study_binomial is for discrete features
#'
#' @param mem Matrix of memory strengths.
#' @param nFeatures Number of features the memory for each item has
#' @param LR Learning Rate. Gives the probability of learning a new features
#'  through practice
#' @param FR Forgetting Rate. Gives the probability of forgetting a feature
#'
#' @return Matrix of memory strengths updated after restudying
#' @export
study_binomial <- function(mem, nFeatures, LR, FR = NULL) {

  mxn <- prod(dim(mem))
  mem <- mem + rbinom(mxn, nFeatures - mem, LR)
  if (!is.null(FR) && FR != 0) {
    mem <- mem - rbinom(mxn, mem, FR)
  }

  return(mem)
}

#' @describeIn test \code{test_binomial} is designed to be used with a matrix of
#' discrete feature and threshold values (represented as integers) drawn from
#' a binomial distribution
#' @inheritParams test_beta
#' @param thresh Matrix of memory thresholds. Threshold is the number of active features
#' needed to support pattern completetion. Must hold integer values.
#' @param acc Logical matrix of performance for previous test
#' @param TR Threshold Reduction rate. Describes the probability of removing the need
#' for a remembering a particular feature following successfull recall
#' @export
test_binomial <- function(mem, nFeatures, thresh, acc, LR, TR, FR=NULL) {

#   strengths <- mem #copy strengths and thresholds from practice test
#   theta  <- thresh
  mxn <- prod(dim(mem))
  nCor = sum(acc)
  mem[acc] <- mem[acc] + rbinom(nCor, nFeatures - mem[acc], LR)
  thresh[acc] <- thresh[acc] - rbinom(nCor, thresh[acc], TR)
  if (!is.null(FR) && FR != 0) {
    mem <- mem - rbinom(mxn, mem, FR)
  }

  return(list(mem = mem,thresh = thresh))
}


#' Reduce the number of memory features activated by retrieval cues in the PCR
#' model.
#' @title Forget
#' @name forget
#' @rdname forget
#' @param mem Matrix of memory strengths.
#' @param FR Forgetting Rate. Gives the probability of forgetting a feature
#'
#' @return Matrix of memory strengths updated after forgetting
#' @export
forget_binomial <- function(mem, FR) {
  mxn <- prod(dim(mem))
  mem <- mem - rbinom(mxn, mem, FR)
  return(mem)
}

#' @rdname forget
#' @export
forget_beta <- function(mem, FR) {
  mxn <- prod(dim(mem))
  mem[mem < 1] <- 1
  binomVAR <-  mem*FR*(1-FR)
  binomM <-  mem*FR
  beta_pars <- betaParams(mean = binomM/mem, sd = sqrt(binomVAR/(mem^2)))
  mem <- mem - (rbeta(mxn, beta_pars$a, beta_pars$b) * mem)
  return(mem)
}
