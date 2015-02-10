#' Function to read a file with individual case data from file.
#'
#' The file needs to be a CSV file with at least columns the columns:
#' DoD (Day of Death) and DoR (Day of Registration) and age (Age of individual who died).
#' All Dates need to be in ISO 8601 format, i.e. YYYY-MM-DD.
#'
#' @param dateFormat Format string specifying how the dates are formatted. Default: \code{"\%Y-\%m-\%d"}.
#' @import ISOweek
#' @return Returns a list containing the elements
#' \tabular{ll}{
#' momo \tab \code{data.frame} with all the relevant data. Note that only numberOfSeasons are kept. \cr
#' dWeeks \tab Vector of mondays corresponding to the weeks spanning the data \cr
#' dLastFull \tab Monday of the last full week in the data.
#' }
#' @export
readmomofile <- function(dateFormat="%Y-%m-%d") {
  #Extract the options.
  euromomoCntrl<-getOption("euromomo")

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
  if (sum(negativeDelay, na.rm=TRUE) > 0) {
    warning("No. of observations with a negative delay: ",sum(negativeDelay),". These observations are removed.\n")
    momo <- subset(momo, !negativeDelay)
  }
  if (sum(is.na(momo$Delay)) > 0) {
    warning("No. of observations with delay equal to NA: ",sum(is.na(momo$Delay)),". These observations are removed.\n")
    momo <- subset(momo, !is.na(momo$Delay))
  }

  #Monday of last full week before DayOfAggregation (equal to DayOfAggregation if its a Sunday)
  #dLastFullWeek <- as.Date(euromomoCntrl$DayOfAggregation) - ifelse(weekday == 6, 6-1, (weekday - 1) + 7)
  #Monday of last full week before DayOfAggregation.
  #Example: Sun, 2014-11-30 -> Mon, 2014-11-17
  weekdayDOA <- ISOweek::ISOweekday(euromomoCntrl$DayOfAggregation)
  dLastFullWeek <- as.Date(euromomoCntrl$DayOfAggregation) - 7 - (weekdayDOA - 1)

  #All observations arriving after DayOfAggregation need to be removed.
  #Actually, its not DayOfAggregation, but those with a DoR which is
  #in the last full week before DayOfAggregation
  cat("Removing ",sum(momo$DoRMon > dLastFullWeek)," observations reported after DayOfAggregation=",as.character(euromomoCntrl$DayOfAggregation)," (i.e. DoRMon > (dLastFullWeek=",as.character(dLastFullWeek),")).\n")
  momo <- subset(momo, momo$DoRMon <= dLastFullWeek)

  # What is the start of the season of the last full week (i.e. last week we have full set of delay==0 observations)
  ISOSeason <- ISOseasonStart(ISOweek(dLastFullWeek))
  # what is the maximum length of observation
  backs<-sapply(euromomoCntrl$groups,function(a) as.numeric(a["back"]))
  #print(backs)
  back<-max(backs)
  # what is the start of the season for the last complete week (i.e. last week we have all observations)
  ISOFSeason<-ISOseasonStart(ISOweek(dLastFullWeek-7*back))
  # what is the start of the last season we have observed fully
  ISOCSeason<-ISOseasonStart(ISOweek(ISOweek2date(paste(ISOFSeason,"-1",sep=""))-1))

  cat("Current season:",ISOSeason,"\nSeason of last complete week:",ISOFSeason,"\nLast Full season:",ISOCSeason,"\n",sep="")

  # Start is at the start of season BaselineSeasons ago
  dStart <- paste(ISOyear(ISOCSeason) - as.numeric(euromomoCntrl$BaselineSeasons), "-W", ISOwoy(ISOCSeason), "-1", sep="")
  dStart <- ISOweek2date(dStart)

  # First week with any observations
  firstWeekInData <- min(momo$DoDMon) - (ISOweek::ISOweekday(min(momo$DoDMon)) - 1)

  # Check
  if (firstWeekInData > dStart) {
    stop("Data don't go back as far as requested.")
  }
  
  #Subset data to be only observations with DoD
  dWeeks <- seq(dStart, dLastFullWeek, by="1 week")
  insideWeeks <- (momo$DoDMon >= dStart) & (momo$DoDMon <= dLastFullWeek)
  cat("Removing",sum(!insideWeeks), "observations with DoDMon not within", paste(dStart,"-",dLastFullWeek)," (i.e. not within",paste0(ISOweek::ISOweek(c(dStart,dLastFullWeek)),collapse=" - "),").\n")
  momo <- subset(momo, insideWeeks)


  return(list(momo=momo,dWeeks=dWeeks, dLastFullWeek=dLastFullWeek))
}
