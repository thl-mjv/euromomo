### Example of the workflow in the country
# library("euromomo")
#Source in all R files as long as it's not a package
sapply(list.files(path="R",pattern="*.R",full.names=TRUE), function(x) {
  try(source(x))
})

### Now using the options
parseDefaultsFile("defaults-example.txt")
checkOptions()

# Create the working directory
week.dir<-directories(debugmode=TRUE)
# (using debugmode creates the working directory to a temporary location)

# no longer needed: euromomoCntrl <- getOption("euromomo")

# Read in the raw data
momoFile <- readmomofile(getOption("euromomo"))

#Create the groups (as stored in the option file)
momo <- makeGroups(momoFile$momo)

### read holidays HERE
holiday.file<-holiday(holiday.filename=getOption("euromomo")$HolidayFile)
### actually these names are deduced from the defaults
groups<-names(getOption("euromomo")[["groups"]])
#groups<-c("momodefault1","momodefault2","momodefault3","momodefault4","momodefault5")
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
                       delay=back,
                       last=getOption("euromomo")$DayOfAggregation)
  summary(data2)

  # Estimate baseline
  data3 <- baseline(data2)
  tail(data3)

  # Calculate Z-scores
  data4 <- zscore(data3)

  # Calculate excess
  data5 <- excess(data4,type="baseli")
  #tail(data5)

  # Generate output
  output(data5)

  # Store the results
  results.list[[i]]<-data5
}

final<-do.call("rbind",results.list)
summary(final)
