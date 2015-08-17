cuedRecall <- function(mem=NULL,thresh=NULL,space=NULL,Tmin=NULL,Tmax=NULL,
                       Time=NULL,lambda=NULL) {

  if (is.null(Time)) {
    recalled <- mem > thresh
  } else {
    RT=Tmin + (Tmax-Tmin)*exp(-lambda*abs(mem-thresh))
    recalled <- (RT < Time) & (mem > thresh)
  }

  if (!is.null(space)) {
    spaced <- as.logical(rbinom(recalled,1,space))
    recalled[recalled & spaced] <- FALSE
  }

  return(recalled)
}

freeRecall <- function(mem=NULL,thresh=NULL,space=NULL,Tmin=NULL,Tmax=NULL,
                       Time=NULL,lambda=NULL) {
  if (is.null(time)) {
    recalled <- mem >= thresh
    serialOrder <- t(apply(mem,1, order, decreasing = TRUE))
    return(list(Acc=recalled, order=serialOrder))
  } else {
    RT <- serialOrder <- matrix(NA, nrow = nrow(mem), ncol = ncol(mem))
    recalled <- matrix(FALSE, nrow = nrow(RT), ncol = ncol(RT))
    for (i in 1:nrow(RT)) {
      ord <- order(mem[i,],decreasing=TRUE)
      reverseOrd <- order(ord)
      CRT <- cumsum(Tmin + (Tmax-Tmin)*exp(-lambda*abs(mem[i,]-thresh[i,]))[ord])
      acc <- CRT < Time & (mem[i,ord]  >= thresh[i,ord])
      if (any(acc)) {
        RT[i,ord[acc]] <- c(CRT[acc][1],diff(CRT[acc]))
      }
      recalled[i,] <- acc[reverseOrd]
      serialOrder[i,] <- ord
    }
  }

  return(list(Acc=recalled,RTrounded=RT,order=serialOrder))

  # tests
  # max(mem[i,])==mem[i,ord[1]] & min(mem[i,])==mem[i,ord[15]]
  # mem[i,ord][reverseOrd] == mem[i,]
  # all((mem[i,] >= thresh[i,])==recalled[i,])
  # all(!is.na(RT[i,]) ==recalled[i,] & is.na(RT[i,]) == !recalled[i,])

}
