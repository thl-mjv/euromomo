### READ DATA IN
readmomofile <- function(euromomoCntrl) {
  #ISO format.
  dateFormat <- "%Y-%m-%d"

  #Read data from file
  momo <- read.csv(euromomoCntrl$InputFile,sep=";",stringsAsFactors=FALSE)
  cat("Read ",nrow(momo)," observations from ",euromomoCntrl$InputFile,".\n")

  #Massage data formatting to Date and adding ISO week.
  momo <- within(momo, {
    #Convert to Date objects. Specific format depends on the input. Parameter?
    DoD <- as.Date(DoD,format=dateFormat)
    DoR <- as.Date(DoR,format=dateFormat)

    #Find ISO week and ISO year of each observation.
    YWoDi <- ISOweek::ISOweek(DoD)
    YWoRi <- ISOweek::ISOweek(DoR)

    #Move days to monday
    DoDMon <- DoD - (ISOweek::ISOweekday(DoD) - 1)
    DoRMon <- DoR - (ISOweek::ISOweekday(DoR) - 1)

    #Compute Delay (measured in number of ISOweeks). If Delay > backWeeks
    #then fix the delay to be equal to backWeeks
    Delay <- (DoRMon - DoDMon)/7
  })

  #Some quality control of the delay slot
  negativeDelay <- momo$Delay < 0
  if (sum(negativeDelay) > 0) {
    warning("No. of observations with a negative delay: ",sum(negativeDelay),". These observations are removed.\n")
    momo <- subset(momo, !negativeDelay)
  }

  #Monday of last full week before DayOfAggregation (equal to DayOfAggregation if its a monday)
  weekday <- ISOweek::ISOweekday(euromomoCntrl$DayOfAggregation)
  dLastFullWeek <- as.Date(euromomoCntrl$DayOfAggregation) - ifelse(weekday == 6, 6-1, (weekday - 1) + 7)

  #All observations arriving after DayOfAggregation need to be removed.
  #Actually, its not DayOfAggregation, but those with a DoR which is
  #in the last full week before DayOfAggregation
  cat("Removing ",sum(momo$DoRMon > dLastFullWeek)," observations reported after DayOfAggregation=",as.character(euromomoCntrl$DayOfAggregation),".\n")
  momo <- subset(momo, momo$DoRMon <= dLastFullWeek)

  #This should be a parameter somewhere and needs to be synced with EuroMomo
  #parameter file. For now: Go back 5 years and then to closest monday more
  #than 5 years ago.
  #dStart <- seq(dLastFullWeek,length=2,by="-6 years")[-1]
#   momo$YoDi<-as.numeric(substring(as.character(momo$YWoDi),7))

#   momo$YoDi <- ISOyear(momo$YWoDi)
#   momo$WoDi <- ISOwoy(momo$YWoDi)


  ISOSeason <- ISOseasonStart(ISOweek(dLastFullWeek))

  dStart <- paste(ISOyear(ISOSeason) - as.numeric(euromomoCntrl$BaselineSeasons), "-W", ISOwoy(ISOSeason), "-1", sep="")
  dStart <- ISOweek2date(dStart)

  firstWeekInData <- min(momo$DoDMon) - (ISOweek::ISOweekday(min(momo$DoDMon)) - 1)
#   dStart <- firstWeekInData
#   dStart <- dStart - (ISOweek::ISOweekday(dStart) - 1)
  #browser()
  if (firstWeekInData > dStart) {
    stop("Data don't go back as far as requested.")
  }
  #Subset data to be only observations with DoD
  dWeeks <- seq(dStart, dLastFullWeek, by="1 week")
  insideWeeks <- (momo$DoDMon >= dStart) & (momo$DoDMon <= dLastFullWeek)
  cat("Removing",sum(!insideWeeks), "observations not within", paste(dStart,"-",dLastFullWeek),".\n")
  momo <- subset(momo, insideWeeks)


  return(list(momo=momo,dWeeks=dWeeks, dLastFullWeek=dLastFullWeek))
}
