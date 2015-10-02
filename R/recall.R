#' PCL Cued Recall Simulation
#'
#' @param mem
#' @param thresh
#' @param space
#' @param Tmin
#' @param Tmax
#' @param Time
#' @param lambda
#'
#' @return Logical Matrix representing recall success/failure of each list item
#'  for each simulation
#' @export
#'
cuedRecall <- function(mem, thresh, space=NULL,Tmin=NULL,Tmax=NULL,
                       Time=NULL,lambda=NULL) {

  if (is.null(Time)) {
    recalled <- mem > thresh
  } else {
    RT=Tmin + (Tmax-Tmin)*exp(-lambda*abs(mem-thresh))
    ouput <- list(recalled = (RT < Time) &  (mem >= thresh),
                     recoverable = mem >= thresh)
  }

  if (!is.null(space) && is.list(output)) {
    spaced <- as.logical(rbinom(output$recalled,1,space))
    output$recalled[output$recalled & spaced] <- FALSE
  } else {
    spaced <- as.logical(rbinom(output,1,space))
    output[output & spaced] <- FALSE
  }

  return(output)
}


#' @title PCL Free Recall Simulation
#' @importFrom dplyr first
#' @param mem
#' @param thresh
#' @param space
#' @param Tmin
#' @param Tmax
#' @param Time
#' @param lambda
#'
#' @return If time parameters are used, list of 4 matrices representing
#'  accuracy, recoverability, reaction time, and serial order.
#'  If time parameters are not used, returns list of 2 matrices representing
#'  accuracy and serial order.
#' @export
#'
freeRecall <- function(mem, thresh, space=NULL,Tmin=NULL,Tmax=NULL,
                       Time=NULL,lambda=NULL) {
  if (is.null(time)) {
    recalled <- mem >= thresh
    serialOrder <- t(apply(mem,1, order, decreasing = TRUE))
    return(list(Acc=recalled, order=serialOrder))

  } else {

    # Preallocate return structures
    RT <- matrix(NA, nrow = nrow(mem), ncol = ncol(mem))
    RTcor <- matrix(NA, nrow = nrow(mem), ncol = ncol(mem))
    serialOrder <- matrix(NA, nrow = nrow(mem), ncol = ncol(mem))
    recalled <- matrix(FALSE, nrow = nrow(mem), ncol = ncol(mem))
    recoverable <- matrix(FALSE, nrow =nrow(mem), ncol = ncol(mem))
    for (i in 1:nrow(RT)) {

      # Find search order, and reverse indices
      ord <- order(mem[i,],decreasing=TRUE) # mem order
      reverseOrd <- order(ord) # sim order

      # Calculate RT and accuracy
      rt <- Tmin + (Tmax-Tmin)*exp(-lambda*abs(mem[i,]-thresh[i,])) # rt in sim order
      crt <- cumsum(rt[ord]) # crt in mem order
      rec <- (mem[i,]  >= thresh[i,])[ord] # rec in mem order
      acc <- crt < Time & rec # acc in mem order
      rt_cor <- c(first(crt[acc],default = numeric(0)), diff(crt[acc])) # rt_cor in mem order

      # Fill in the output structures
      # In simulation order, not search order!!!!
      RT[i,] <- rt
      recalled[i,] <- acc[reverseOrd]
      recoverable[i,] <- rec[reverseOrd]
      rt_cor_tmp <- rep(NA,length(rt))
      rt_cor_tmp[acc] <- rt_cor
      RTcor[i,] <- rt_cor_tmp[reverseOrd]
      serialOrder[i,] <- ord
    } # close for
  } # close if switch

  return(list(Acc=recalled,RT=RT, RTcor = RTcor,
              order=serialOrder,recoverable = recoverable))

} # Close freeRecall
