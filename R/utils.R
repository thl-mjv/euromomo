#' Simple example function
#'
#' @param a a parameter
#' @return another value
#' @export
na.0<-function(a,b=0) {
  ifelse(is.na(a),b,a)
}
