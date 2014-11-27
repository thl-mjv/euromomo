#' Simple example function
#'
#' @param a a parameter
#' @param b an alternative
#' @return another value
#' @export
na.0 <- function(a,b=0) {
  ifelse(is.na(a),b,a)
}

#' Extract WOY from an YYYY-WXX representation.
#'
#' Function to extract the week of the year (WOY) as decimal number (00–53)
#' (i.e. not the year, just the number) from an ISOweek::ISOweek generated date.
#' Example: For 2013-W13 this would be 13.
#'
#' @param x A vector of ISOweek::ISOweek generated objects.
#' @return Vector of numeric each being between 00-53.
#' @export
#'
ISOwoy <- function(x) {
  as.numeric(gsub("(^[0-9]+)(-W)([0-9]{2})","\\3",x))
}

#' Function to extract the year of a YYYY-WXX representation.
#'
#' Extract year from a vector of \code{ISOweek::ISOweek} generated dates.
#' Example: For 2013-W13 this would be 2013.
#'
#' @param x A vector of \code{ISOweek::ISOweek} generated objects.
#' @return Vector of numeric containing the year.
#' @export
ISOyear <- function(x) {
  as.numeric(gsub("(^[0-9]+)(-W)([0-9]{2})","\\1",x))
}

#' Function that calculates the start of the season
#' @param x: vector of ISOweek
#' @return a vector of begining of the season
#' @export
ISOseasonStart <- function(x){
  season <- ifelse(ISOwoy(x) >= 40 | ISOwoy(x) <= 20, "Winter", "Summer")

  Year.season <- ifelse(season=="Summer", paste(ISOyear(x), "-W20", sep=""),
                        ifelse(ISOwoy(x)<20, paste((ISOyear(x)-1), "-W40", sep=""),
                               paste(ISOyear(x), "-W40", sep="")))

  return(Year.season)
}

#' Simple check function before pushing.
checkBeforePush <- function() {
  rm(list=ls()) ; source("momomaster.R")
}
