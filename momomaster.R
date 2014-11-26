### Example of the workflow in the country
# library("euromomo")
parseDefaultsFile()
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
result.list<-list()

for(i in groups) {
  rTList <- file2ReportingTriangle(euromomoCntrl) # something about the group
  rTDF <- rT2DataFrame(rTList$cumRT)
  head(rTDF)
  ### OR read holidays HERE
  #holiday.file<-holiday()

  drTDF<-delay(rTDF,method="negbin",holiday=holiday.file)
  tail(drTDF,20)
  data2<-addconditions(data,spring=15:26,autumn=30:48,delay=1:2)
  summary(data2)

  data3 <- baseline(data2)

  head(data3)
  with(data3,as.data.frame(table(CondSeason,CondSomething,CondDelays,CondLength,cond)))


  data4 <- zscore(data3)

  data5 <- excess(data4,type="baseli")



