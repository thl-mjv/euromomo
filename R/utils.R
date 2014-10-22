#' Simple example function
#'
#' @param a a parameter
#' @param b anternative
#' @return another value
#' @export
na.0<-function(a,b=0) {
  ifelse(is.na(a),b,a)
}
