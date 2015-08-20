#' @export
paramBounds <- function(p, lower= 0, upper = 1) {
  if (any(p[names(p) %in% c("ER","LR","TR","F1","F2","FR","theta","space")] > upper, na.rm = TRUE) ||
      any(p[names(p) %in% c("ER","LR","TR","F1","F2","FR","Tmin","lambda","theta","space")] < lower,
          na.rm = TRUE)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' @export
checkRequiredParams <- function(par, fn) {
  reqParams <- c(names(formals(fn)$free), names(formals(fn)$fixed))
  reqParams <- reqParams[!reqParams %in%  c("","Time","Tmin","Tmax","lambda")]
  givenParams <- names(par)
  if (!all( reqParams[reqParams != ""]  %in% givenParams)) {
    stop(paste(reqParams[!reqParams %in% givenParams], " not specified in model,check model input list"))
  }
}
