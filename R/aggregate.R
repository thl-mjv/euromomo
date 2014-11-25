library("ISOweek")

#' Global variable containing euromomo control options. This is dirty and needs to be changed
#' once the handling of parameters is established for the euromomo
#' package.

euromomoCntrl <- list(
  #File with mortality data to read
  #fileName <- "../../../SampleData/CH_INPUT_19NOV14.CSV"
  fileName = "../data/DoD_DoR.txt",
  #Format for how Dates are specified
  #dateFormat <- "%d.%m.%Y"
  dateFormat = "%d%b%Y", #
  #Choose the number of weeks to remove for modeling delay (Parameter?)
  back = 6,
  #Day of aggregation (a thursday)
  dAggregation = ISOweek::ISOweek2date("2014-W01-4")
)

#' R function for reading in and performing the aggregation necessary
#' to get the reporting triangle.
#' @param euromomoCntrl A list containing entries for defining the
#'        operation of the function.
#' @return A list containing the reporting triangle, the cumulated
#' reporting triangle, the time points and the delays.

file2ReportingTriangle <- function(euromomoCntrl) {
  #Extract from options object
  backWeeks <- euromomoCntrl$back

  #Read data from file
  momo <- read.csv(euromomoCntrl$fileName,sep=";",stringsAsFactors=FALSE)
  cat("Read ",nrow(momo)," observations from ",euromomoCntrl$fileName,".\n")

  #Massage data formatting to Date and adding ISO week.
  momo <- within(momo, {
    #Convert to Date objects. Specific format depends on the input. Parameter?
    DoD <- as.Date(DoD,format=euromomoCntrl$dateFormat)
    DoR <- as.Date(DoR,format=euromomoCntrl$dateFormat)

    #Find ISO week and ISO year of each observation.
    YWoDi <- ISOweek::ISOweek(DoD)
    YWoRi <- ISOweek::ISOweek(DoR)

    #Move days to monday
    DoDMon <- DoD - (ISOweek::ISOweekday(DoD) - 1)
    DoRMon <- DoR - (ISOweek::ISOweekday(DoR) - 1)

    #Compute Delay (measured in number of ISOweeks). If Delay > backWeeks
    #then fix the delay to be equal to backWeeks
    Delay <- (DoRMon - DoDMon)/7
    cat("Truncating ",sum(Delay>backWeeks), " observations with a delay >",backWeeks, "weeks to be equal to ",backWeeks," weeks.\n")
    Delay[Delay>backWeeks] <- backWeeks
  })

  #Some quality control of the delay slot
  negativeDelay <- momo$Delay < 0
  if (sum(negativeDelay) > 0) {
    warning("No. of observations with a negative delay: ",sum(negativeDelay),". These observations are removed.\n")
    momo <- subset(momo, !negativeDelay)
  }
  
  #Monday of last full week before dAggregation (equal to dAggregation if its a monday)
  weekday <- ISOweek::ISOweekday(euromomoCntrl$dAggregation)
  dLastFullWeek <- euromomoCntrl$dAggregation - ifelse(weekday == 6, 6-1, (weekday - 1) + 7)

  #All observations arriving after dAggregation need to be removed.
  #Actually, its not dAggregation, but those with a DoR which is
  #in the last full week before dAggregation
  cat("Removing ",sum(momo$DoRMon > dLastFullWeek)," observations reported after dAggregation=",as.character(euromomoCntrl$dAggregation),".\n")
  momo <- subset(momo, momo$DoRMon <= dLastFullWeek)
  
  #This should be a parameter somewhere and needs to be synced with EuroMomo
  #parameter file. For now: Go back 5 years and then to closest monday more
  #than 5 years ago.
  #dStart <- seq(dLastFullWeek,length=2,by="-6 years")[-1]
  firstWeekInData <- min(momo$DoDMon) - (ISOweek::ISOweekday(min(momo$DoDMon)) - 1)
  dStart <- firstWeekInData
  dStart <- dStart - (ISOweek::ISOweekday(dStart) - 1)
  #browser()
  if (firstWeekInData > dStart) {
    stop("Data don't go back as far as requested.")
  }
  #Subset data to be only observations with DoD
  dWeeks <- seq(dStart, dLastFullWeek, by="1 week")
  insideWeeks <- (momo$DoDMon >= dStart) & (momo$DoDMon <= dLastFullWeek)
  cat("Removing",sum(!insideWeeks), "observations not within", paste(dStart,"-",dLastFullWeek),".\n")
  momo <- subset(momo, insideWeeks)

  #Aggregate data between dStart and dLastFullWeek (CHECK WITH EuroMOMO Parameter Definition)
  #to determine reporting triangle
  YWoDiFac <- factor(as.character(momo$YWoDi),levels=as.character(ISOweek::ISOweek(dWeeks)))
  DelayFac <- factor(momo$Delay, levels=0:backWeeks)
  rT <- table(YWoDiFac,DelayFac)

  #Put NA's at position of structural zeroes
  cellAvailable <- outer(dWeeks, 0:backWeeks, function(dWeek,Delay) {
    dWeek + Delay*7 <= dLastFullWeek #euromomoCntrl$dAggregation
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
  return(list(cumRT=cumRT, rT=rT,dWeeks=dWeeks, delays=0:backWeeks))
}

#' Deprecated function to read IRISH data
#aggregateIE <- function() {
#  momo <- foreign::read.dta(file="../../SampleData/delay-Total-Ireland-2014-47.dta")
#  head(momo,n=1)
#}

doIt <- function() {
  source("aggregate.R")
  rTList <- file2ReportingTriangle(euromomoCntrl)
  rTList$rT
  rTList$cumRT
}
