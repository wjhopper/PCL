#' study
#'
#' @param mem Matrix of memory strengths.
#' @param nFeatures Number of features the memory for each item has
#' @param LR Learning Rate. Gives the probability of learning a new features
#'  through practice
#' @param FR Forgetting Rate. Gives the probability of forgetting a feature
#'
#' @return Matrix of memory strengths updated after restudying
#' @export
study <- function(mem, nFeatures, LR, FR = NULL) {

  mxn <- prod(dim(mem))
  strengths <- mem + rbinom(mxn, nFeatures - mem, LR)
  if (!is.null(FR)) {
    strengths <- strengths - rbinom(mxn, strengths, FR)
  }

  return(strengths)
}

#' test
#'
#' @inheritParams study
#' @param thresh Matrix of memory thresholds. Threshold is the number of active features
#'  needed to support pattern completetion
#' @param acc Logical matrix of performance for previous test
#' @param TR Threshold Reduction rate. Describes the probability of removing the need
#'  for a remembering a particular feature following successfull recall
#'
#' @return List of two matrices. mem is a matrix of memory strengths following succcesful
#'  recall. thresh is a mastrix of feature thresholds following succesful recall.
#'
#' @export
test <- function(mem, nFeatures, thresh, acc, LR, TR, FR=NULL) {

  strengths <- mem #copy strengths and thresholds from practice test
  theta  <- thresh
  mxn <- prod(dim(mem))
  nCor = sum(acc)
  strengths[acc] <- mem[acc] + rbinom(nCor,nFeatures - mem[acc], LR)
  theta[acc] <- thresh[acc] - rbinom(nCor, thresh[acc], TR)
  if (!is.null(FR)) {
    strengths <- strengths - rbinom(mxn, strengths, FR)
  }

  return(list(mem = strengths,thresh = theta))
}
