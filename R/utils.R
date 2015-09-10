#' @export
paramBounds <- function(p) {

  probability_params <- c("ER","LR","TR","F1","F2","FR","space","theta")
  transform_params <- "alpha"
  strict_positive_params <- c("Tmin","Tmax","lambda")

  prob_check <- any(p[names(p) %in% probability_params] < 0, na.rm = TRUE) ||
                any(p[names(p) %in% probability_params] > 1, na.rm = TRUE)
  transform_check <- any(p[names(p) %in% probability_params] <= 0, na.rm = TRUE) ||
                     any(p[names(p) %in% probability_params] >= 1, na.rm = TRUE)
  strict_pos_check <- any(p[names(p) %in% strict_positive_params] <= 0, na.rm = TRUE)

  if (any(prob_check, transform_check, strict_pos_check)) {
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
