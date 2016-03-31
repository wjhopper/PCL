forget_binomial <- function(mem, FR) {
  mxn <- prod(dim(mem))
  mem <- mem - rbinom(mxn, mem, FR)
  return(mem)
}

forget_beta <- function(mem, FR) {
  mxn <- prod(dim(mem))
  mem[mem < 1] <- 1
  binomVAR <-  mem*FR*(1-FR)
  binomM <-  mem*FR
  beta_pars <- betaParams(mean = binomM/mem, sd = sqrt(binomVAR/(mem^2)))
  mem <- mem - (rbeta(mxn, beta_pars$a, beta_pars$b) * mem)
  return(mem)
}

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

study_binomial <- function(mem, nFeatures, LR, FR = NULL) {
  mxn <- prod(dim(mem))
  mem <- mem + rbinom(mxn, nFeatures - mem, LR)
  if (!is.null(FR) && FR != 0) {
    mem <- mem - rbinom(mxn, mem, FR)
  }

  return(mem)
}

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

