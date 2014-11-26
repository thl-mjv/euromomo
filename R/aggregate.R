library("ISOweek")


#' R function for reading in and performing the aggregation necessary
#' to get the reporting triangle.
#' @param momo is a data.frame with the complete data
#'        back is the number of weeks for delay-adjustment age group specific
#'        groupindicator is the indicator of the age group for data aggregation
#' @return A list containing the reporting triangle for each age group, the cumulated reporting triangle, the time points and the delays.
#' @export
df2ReportingTriangle <- function(momo, groupindicator, back, dWeeks, dLastFullWeek) {

  # Subsetting for the specific age group
  momo<-subset(momo, groupindicator)

  #Massage data formatting to Date and adding ISO week.
  momo <- within(momo, {

    #Compute Delay (measured in number of ISOweeks). If Delay > backWeeks
    #then fix the delay to be equal to backWeeks
    cat("Truncating ",sum(Delay>back), " observations with a delay >",back, "weeks to be equal to ",back," weeks.\n")
    Delay[Delay>back] <- back
  })


  #Monday of last full week before dAggregation (equal to dAggregation if its a monday)
  #weekday <- ISOweek::ISOweekday(euromomoCntrl$dAggregation)
  #dLastFullWeek <- euromomoCntrl$dAggregation - ifelse(weekday == 6, 6-1, (weekday - 1) + 7)


  #All observations arriving after dAggregation need to be removed.
  #Actually, its not dAggregation, but those with a DoR which is
  #in the last full week before dAggregation
  #cat("Removing ",sum(momo$DoRMon > dLastFullWeek)," observations reported after dAggregation=",as.character(euromomoCntrl$dAggregation),".\n")
  #momo <- subset(momo, momo$DoRMon <= dLastFullWeek)

#   #This should be a parameter somewhere and needs to be synced with EuroMomo
#   #parameter file. For now: Go back 5 years and then to closest monday more
#   #than 5 years ago.
#   #dStart <- seq(dLastFullWeek,length=2,by="-6 years")[-1]
#   firstWeekInData <- min(momo$DoDMon) - (ISOweek::ISOweekday(min(momo$DoDMon)) - 1)
#   dStart <- firstWeekInData
#   dStart <- dStart - (ISOweek::ISOweekday(dStart) - 1)
#   #browser()
#   if (firstWeekInData > dStart) {
#     stop("Data don't go back as far as requested.")
#   }
#   #Subset data to be only observations with DoD
#   dWeeks <- seq(dStart, dLastFullWeek, by="1 week")
#   insideWeeks <- (momo$DoDMon >= dStart) & (momo$DoDMon <= dLastFullWeek)
#   cat("Removing",sum(!insideWeeks), "observations not within", paste(dStart,"-",dLastFullWeek),".\n")
#   momo <- subset(momo, insideWeeks)

  #Aggregate data between dStart and dLastFullWeek (CHECK WITH EuroMOMO Parameter Definition)
  #to determine reporting triangle
  YWoDiFac <- factor(as.character(momo$YWoDi),levels=as.character(ISOweek::ISOweek(dWeeks)))
  DelayFac <- factor(momo$Delay, levels=0:back)
  rT <- table(YWoDiFac,DelayFac)

  #Put NA's at position of structural zeroes

  cellAvailable <- outer(dWeeks, 0:back, function(dWeeks,Delay) {
    dWeeks + Delay*7 <= dLastFullWeek
  })

  #Sanity checks
  if (sum(rT[!cellAvailable]) != 0) {
    stop("rT is not zero at NA cells!")
  }
  rT[!cellAvailable] <- NA
  if (sum(rT,na.rm=TRUE) != nrow(momo)) {
    stop("Number of obs. in rT does not match rows in 'momo'.\n")
  }
  #Cumulated reporting triangle
  cumRT <- t(apply(rT, MARGIN=1, cumsum))

  cat("Tabulated a total of ",sum(rT,na.rm=TRUE),"observations in the reporting triangle.\n")
  #Done
  return(list(cumRT=cumRT, rT=rT,dWeeks=dWeeks, delays=0:back))
}

# Deprecated function to read IRISH data
#aggregateIE <- function() {
#  momo <- foreign::read.dta(file="../../SampleData/delay-Total-Ireland-2014-47.dta")
#  head(momo,n=1)
#}

#' Turn a triangle into a data.frame
#' @param rT an output from df2ReportingTriangle
#' @return a data frame with column ISOweek and one column for each delay band
#' @export
rT2DataFrame <- function(rT) {
  colnames(rT) = paste("w",sprintf("%02d",as.numeric(colnames(rT))),sep="")
  df <- as.data.frame(rT)
  df <- cbind(ISOweek=rownames(df),df)
  rownames(df) <- seq_len(nrow(df))
  return(df)
}

#' Show the delay as a function of time
#' @param df a data frame
#' @param back number of weeks in delay
#' @return nothing
#' @export
plotDelay <- function(df, back) {
  delayIdx <-  grep("^w[0-9]+$",colnames(df))
  #maxDelay <- length(delayIdx) - 1
  total <- df[,max(delayIdx)]
  matplot(1:nrow(df), df[,delayIdx]/matrix(total,nrow=nrow(df),ncol=back+1,byrow=FALSE),type="l",lty=1,ylab="Proportion of total",xlab="Time",ylim=c(0,1))
  invisible(NULL)
}

