#' Draw vertical line indicating start of the delay estimation.
#'
#' This internal helper function draws a vertical line and adds an axis label
#' at the top of the graph.
#'
#' @param mondays Vector of dates indicating the mondays of the ISOweeks.
#'
verticalLineSDE <- function(mondays) {
  dStart <- ISOweek2date(paste0(options("euromomo")$euromomo$StartDelayEst,"-1"))
  if (dStart %in% mondays) {
    idx <- which(mondays == dStart)
    lines( rep(mondays[idx],2), c(0,1e99),lwd=2,lty=2)
    axis(3, at=mondays[idx], labels="StartDelayEst",cex.axis=0.7,lwd=2,las=1)
  }
}

#' Show the delay as a function of time.
#'
#' @param df a data frame representing a reporting triangle.
#' @param main Title of the plot (typically the label of the agegroup)
#' @return nothing (NULL)
#' @export
plotDelay <- function(df, main=NULL) {
  delayIdx <-  grep("^w[0-9]+$",colnames(df))
  maxDelay <- length(delayIdx) - 1
  total <- df[,max(delayIdx)]

  mondays <- ISOweek2date(paste0(df$ISOweek,"-1"))
  #Matplot doesn't handle dates in the x-axis formatting. Using plot followed matlines
  plot(mondays,rep(0,length(mondays)),ylab="Proportion of total",xlab="Time of death",ylim=c(0,1),main=main,type="n")
  matlines(mondays, df[,delayIdx]/matrix(total,nrow=nrow(df),ncol=maxDelay+1,byrow=FALSE),type="l",lty=1)
  verticalLineSDE(mondays)
  legend(x="bottom", ncol=5, paste0(0:maxDelay," weeks"), lty=1,col=seq_len(maxDelay+1),bg="white")

  invisible(NULL)
}


#' Illustrate empirical delay distribution.
#'
#' Show empirical and model based median of delay distribution as a
#' function of occurence time t.
#'
#' @param rT - reporting triangle as it would be at the end.
#' @param w - half-width of moving window
#' @param ISOweeks Vector of ISO week dates where to show the result
#' @param quantiles - which quantiles to show
#' @param col Color of the quantiles, see \code{\link{plot}}.
#' @param lty Line types, see \code{\link{plot}}.
#' @param lwd Line width, see \code{\link{plot}}.
#' @param main - Title of the plot (tpyically the label of the age group)
#' @return Nothing
#' @export
plotDelayQuantiles <- function(rT, w=1, ISOweeks, quantiles=c(0.1,0.5,0.9),col=1,lty=1,lwd=2, main=NULL) {

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
  plot(mondays, quants[,1],xlab="Time of death",ylab="Reporting Delay (weeks)",ylim=c(0,maxDelay),type="l",col=col[1],lty=lty[1],lwd=lwd[1],main=main)
  #Add grid for easier reading.
  grid(ny=NULL,nx=NA,lwd=2)
  #void <- sapply(seq(0,maxDelay,by=2), function(i) {
  #  abline(a=i,b=0,col="lightgray",lty=2)
  #})
  axis(2,at=0:maxDelay,labels=FALSE,tcl=-0.25)
  #Add additional lines.
  if (length(quantiles)>1) {
    matlines(mondays, quants[,-1],type="l",col=col[-1],lty=lty[-1],lwd=lwd[-1])
  }
  #Draw vertical line indicating start of delay estimation.
  verticalLineSDE(mondays)


  #Make a legend
  expressions <- lapply(quantiles, function(quantile) substitute(expression(q[x]),list(x=quantile)))
  legend(x="bottomleft",legend=sapply(expressions,eval),lty=lty,col=col,lwd=lwd,bg="white")

  #Done
  invisible(NULL)
}

#' A function combining the plotDelay and plotDelay quantile function
#'
#' @param rTList List containing different versions of the reporting triangle data.frame
#' @param w Size of the moving window used for smoothing
#' @param quantiles Vector containing the quantiles to show in the 2nd plot of the delay distribution.
#' @param main Title of the graphics (typically the label of the age group)
#' @param week.dir Directory name where all output is stored.
#' @export
plotDelayDiagnostics2File <- function(rTList, w=1, quantiles=c(0.25,0.50,0.75,0.9,0.95,0.99), main=NULL, week.dir) {
  #Setup filename and open pdf device (could have been png also)
  fileName <- paste0("Delay-", groupOpts["label"], ".pdf")
  pdf(file = file.path(week.dir, "diagnostics", fileName), onefile=TRUE, width=8,height=6)

  #Call the two plot routines.-
  plotDelay(rT2DataFrame(rTList$cumRT), main=main)
  rTDF <- rT2DataFrame(rTList$rT)
  plotDelayQuantiles(rTDF, w=w, ISOweeks=rTDF$ISOweek,
                     quantiles=quantiles,
                     lty=seq_len(length(quantiles)),
                     lwd=(seq_len(length(quantiles))+1) %/% 2,
                     col=seq_len(length(quantiles)),main=main)

  #Close device
  dev.off()
}
