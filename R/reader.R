### READ DATA IN
readmomofile <- function(euromomoCntrl) {
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
  wAggregation<-as.numeric(substring(ISOweek::ISOweek(euromomoCntrl$dAggregation),7))
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
  momo$YoDi<-as.numeric(substring(as.character(momo$YWoDi),7))
  momo$WoDi<-as.numeric(substring(as.character(momo$YWoDi),1,4))
  firstYearInData<-as.numeric(format(euromomoCntrl$dAggregation,"%Y"))+(wAggregation>=40)-euromomoCntrl$back
  #firstWeekInData <- min(momo$DoDMon) - (ISOweek::ISOweekday(min(momo$DoDMon)) - 1)
  firstWeekInData<-as.Date(paste(firstYearInData,"-01-01",sep=""))
  dStart <- firstWeekInData
  dStart <- dStart - (ISOweek::ISOweekday(dStart) - 1)
  #browser()
  #if (firstWeekInData > dStart) {
  #  stop("Data don't go back as far as requested.")
  #}
  #Subset data to be only observations with DoD
  dWeeks <- seq(dStart, dLastFullWeek, by="1 week")
  insideWeeks <- (momo$DoDMon >= dStart) & (momo$DoDMon <= dLastFullWeek)
  cat("Removing",sum(!insideWeeks), "observations not within", paste(dStart,"-",dLastFullWeek),".\n")
  momo <- subset(momo, insideWeeks)

  return(momo)
}
