#' Show the delay as a function of time.
#'
#' @param df a data frame representing a reporting triangle.
#' @return nothing (NULL)
#' @export
plotDelay <- function(df) {
  delayIdx <-  grep("^w[0-9]+$",colnames(df))
  maxDelay <- length(delayIdx) - 1
  total <- df[,max(delayIdx)]
  matplot(1:nrow(df), df[,delayIdx]/matrix(total,nrow=nrow(df),ncol=maxDelay+1,byrow=FALSE),type="l",lty=1,ylab="Proportion of total",xlab="Time",ylim=c(0,1))
  invisible(NULL)
}


#' Illustrate empirical delay distribution.
#'
#' Show empirical and model based median of delay distribution as a
#' function of occurence time t.
#'
#' @param rT - reporting triangle as it would be at the end
#' @param date - vector of dates where to show the result
#' @param w - half-width of moving window
#' @param quantiles - which quantiles to show
#' @return Nothing
#' @export
plotDelayQuantiles <- function(rT, w=1, ISOweeks, quantiles=c(0.1,0.5,0.9),col=1,lty=1,lwd=2) {

  #Ensure correct length of col, lty and lwd by recycling first argument
  if (length(col) != length(quantiles)) { col <- rep(col[1],length(quantiles))}
  if (length(lty) != length(quantiles)) { lty <- rep(lty[1],length(quantiles))}
  if (length(lwd) != length(quantiles)) { lwd <- rep(lwd[1],length(quantiles))}

  #Determine max delay from the reporting triangle.
  delayIdx <-  grep("^w[0-9]+$",colnames(rT))
  maxDelay <- length(delayIdx) - 1
  #Initialize time varying PMF data structure
  res <- matrix(NA, nrow=length(ISOweeks), ncol=maxDelay+1)

  #which data variables are actually in rT
  isThere <- !is.na(sapply(ISOweeks, function(week) pmatch(as.character(week),rT$ISOweek)))
  idx <- which(isThere)

  #Loop over all time points and determine quantile in unadjusted empirical
  #delay distribution.
  for (i in (w+min(idx)):(max(idx)-w)) {
    now <- ISOweeks[i]
    idx <- pmatch(as.character(now),rT$ISOweek)
    subset <- rT[idx + c(-w:w),delayIdx,drop=FALSE]
    res[i,] <- colSums(subset) / sum(subset)
  }

  #A slightly modified function to determine quantiles, which can
  #handle NAs (i.e. if no case at all)
  quantile <- function(q) {
    apply(res, 1, function(x) {
      if (all(is.na(x))) return(NA) else return(which.max(cumsum(x) >= q) - 1)
    })
  }

  #Find the specified quantiles of the delay distribution
  quants <- sapply(quantiles, quantile)

  #Make a plot (use plot.Dates instead of matplot)
  mondays <- ISOweek2date(paste0(ISOweeks,"-1"))
  plot(mondays, quants[,1],xlab="Time of death",ylab="Reporting Delay (weeks)",ylim=c(0,maxDelay),type="l",col=col[1],lty=lty[1],lwd=lwd[1])
  if (length(quantiles)>1) {
    matlines(mondays, quants[,-1],type="l",col=col[-1],lty=lty[-1],lwd=lwd[-1])
  }

  #Make a legend
  expressions <- sapply(quantiles, function(quantile) substitute(q[x],list(x=quantile)))
  legend(x="bottomleft",legend=expressions,lty=lty,col=col,lwd=lwd)

  #Done
  invisible(NULL)
}
