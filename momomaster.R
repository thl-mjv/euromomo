### Example of the workflow in the country
# library("euromomo")
# For testing source all function definitions
(files<-list.files("R/",pattern="*.R",full=TRUE))
for(i in files) source(i)
#parseDefaultsFile()
# Options for the aggregation
euromomoCntrl <- list(
  #File with mortality data to read
  #fileName <- "../../../SampleData/CH_INPUT_19NOV14.CSV"
  fileName = "data/DoD_DoR.txt",
  #Format for how Dates are specified
  #dateFormat <- "%d.%m.%Y"
  dateFormat = "%Y-%m-%d", #
  #Choose the number of weeks to remove for modeling delay (Parameter?)
  back = 6,
  #Day of aggregation (a thursday)
  dAggregation = ISOweek::ISOweek2date("2013-W01-4"),
  #Standard number of working days in a week.
  nWorkdays=5
)

### Something goes here to see that

### read holidays HERE
holiday.file<-holiday()
### actually these names are deduced from the defaults
groups<-c("Total")
results.list<-list()

#momo<-readmomofile(euromomoCntrl)
#head(momo)

for(i in groups) {
  #i<-"Total"
  rTList <- file2ReportingTriangle(euromomoCntrl) # something about the group
  rTDF <- rT2DataFrame(rTList$cumRT)
  head(rTDF)
  ### OR read holidays HERE
  #holiday.file<-holiday()

  # Delay adjustment
  drTDF<-delay(rTDF,method="negbin",holiday=holiday.file)
  tail(drTDF,20)

  # Add conditions for the baseline estimation
  data2<-addconditions(drTDF,spring=15:26,autumn=30:48,delay=6)
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
