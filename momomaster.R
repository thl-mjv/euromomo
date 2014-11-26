### Example of the workflow in the country
# library("euromomo")
#Source in all R files as long as it's not a package
sapply(list.files(path="R",pattern="*.R",full.names=TRUE), function(x) {
  try(source(x))
})

### Now using the options
parseDefaultsFile("defaults-example.txt")
checkOptions()

#Change to working directory
#setwd( getOption("euromomo")$WorkDirectory)
## using debugmode creates the working directory to a temporary location
week.dir<-directories(debugmode=TRUE)
euromomoCntrl <- getOption("euromomo")
momoFile <- readmomofile(getOption("euromomo"))

#Create the groups (as stored in the option file)
momo <- makeGroups(momoFile$momo)

# # Options for the aggregation
# euromomoCntrl <- list(
#   #File with mortality data to read
#   #fileName <- "../../../SampleData/CH_INPUT_19NOV14.CSV"
#   fileName = "data/DoD_DoR.txt",
#   #Format for how Dates are specified
#   #dateFormat <- "%d.%m.%Y"
#   dateFormat = "%Y-%m-%d", #
#   #Choose the number of weeks to remove for modeling delay (Parameter?)
#   back = 6,
#   #Day of aggregation (a thursday)
#   dAggregation = ISOweek::ISOweek2date("2013-W01-4"),
#   #Standard number of working days in a week.
#   nWorkdays=5
# )

### Something goes here to see that

### read holidays HERE
holiday.file<-holiday(holiday.filename=getOption("euromomo")$HolidayFile)
### actually these names are deduced from the defaults
groups<-c("momodefault5")
results.list<-list()

for(i in groups) {
  #i<-"Total"
  groupOpts <- getOption("euromomo")[["groups"]][[i]]

  #rTList <- file2ReportingTriangle(getOption("euromomo")) # something about the group
  #Define nre function df2Reportiangle

  groupIndicator <- momo[, paste("group_",i,sep="")]
  back<-as.numeric(groupOpts["back"])


  rTList <- df2ReportingTriangle(momo, groupIndicator, back, dWeeks=momoFile$dWeeks, dLastFullWeek=momoFile$dLastFullWeek) # something about the group

  rTDF <- rT2DataFrame(rTList$cumRT)
  head(rTDF)
  ### OR read holidays HERE
  #holiday.file<-holiday()

  # Delay adjustment
  drTDF<-delay(rTDF,method="negbin",holiday=holiday.file)
  tail(drTDF,20)

  # Add conditions for the baseline estimation
  data2<-addconditions(drTDF,
                       spring=getOption("euromomo")$spring,
                       autumn=getOption("euromomo")$autumn,
                       delay=back)
  summary(data2)

  # Estimate baseline
  data3 <- baseline(data2)
  tail(data3)

  # Calculate Z-scores
  data4 <- zscore(data3)

  # Calculate excess
  data5 <- excess(data4,type="baseli")
  tail(data5)

  # MISSING: format the output
  # Store the results
  results.list[[i]]<-data5
}

final<-do.call("rbind",results.list)
summary(final)
