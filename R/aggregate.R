library("ISOweek")

#' Global variable. This is dirty and needs to be changed
#' once the handling of parameters is established for the euromomo
#' package.

euromomoCntrl <- list(
  #File with mortality data to read
  #fileName <- "../../SampleData/CH_INPUT_19NOV14.CSV"
  fileName = "../../SampleData/DoD_DoR.txt",
  #Format for how Dates are specified
  #dateFormat <- "%d.%m.%Y"
  dateFormat = "%d%b%Y", #
  #Choose the number of weeks to remove for modeling delay (Parameter?)
  back = 6,
  #Day of aggregation (a thursday)
  dAggregation = ISOweek::ISOweek2date("2014-W01-4")
)

#' R function for reading in and performing the aggregation.
#' @param euromomoCntrl A list containing entries for defining the
#'        operation of the function.
#' @return A data.frame.
#'

aggregate <- function(euromomoCntrl) {
  #Extract from options object
  backWeeks <- euromomoCntrl$back

  #Read data from file
  momo <- read.csv(euromomoCntrl$fileName,sep=";",stringsAsFactors=FALSE)
  cat("Read ",nrow(momo)," lines of data from ",euromomoCntrl$fileName,".\n")

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
    cat("Truncating ",sum(Delay>backWeeks), " observations with a delay >",backWeeks, "weeks.\n")
    Delay[Delay>backWeeks] <- backWeeks
  })

  #All observations arriving after dAggregation need to be removed
  cat("Removing ",sum(momo$DoR > euromomoCntrl$dAggregation)," observations reported after dAggregation.\n")
  momo <- subset(momo, momo$DoR <= euromomoCntrl$dAggregation)

  #Monday of last full week before dAggregation (equal to dAggregation if its a monday)
  dLastFullWeek <- with(euromomoCntrl, {
    weekday <- ISOweek::ISOweekday(dAggregation)
    dAggregation - ifelse(weekday == 1, 0, (weekday - 1) + 7)
  })

  #This should be a parameter somewhere and needs to be synced with EuroMomo
  #parameter file. For now: Go back 5 years and then to closest monday more
  #than 5 years ago.
  dStart <- seq(dLastFullWeek,length=2,by="-5 years")[-1]
  dStart <- dStart - (ISOweek::ISOweekday(dStart) - 1)
  if (min(momo$DoDMon) > dStart) {
    stop("Data don't go back as far as requested.")
  }

  #Aggregate data between dBack and dLastFullWeek (CHECK WITH EuroMOMO Parameter Definition)
  dWeeks <- seq(dBack, dLastFullWeek, by="1 week")
  YWoDiFac <- factor(as.character(momo$YWoDi),levels=as.character(ISOweek::ISOweek(dWeeks)))
  DelayFac <- factor(momo$Delay, levels=0:back)

  #Determine reporting triangle by aggreation
  rT <- table(YWoDiFac,DelayFac)

  #Put NA's at position of structural zeroes
  cellAvailable <- outer(dWeeks, 0:back, function(dWeek,Delay) {
    dWeek + Delay*7 <= euromomoCntrl$dAggregation
  })

  #Sanity check
  if (sum(rT[!cellAvailable]) != 0) {
    stop("rT is not zero at NA cells!")
  }
  rT[!cellAvailable] <- NA

  #Cumulated reporting triangle
  cumRT <- t(apply(rT, MARGIN=1, cumsum))

  #Done
  return(list(cumRT=cumRT, week=week, delays=0:back))
}

#' Deprecated function to read IRISH data
aggregateIE <- function() {
  momo <- foreign::read.dta(file="../../SampleData/delay-Total-Ireland-2014-47.dta")
  head(momo,n=1)
}
