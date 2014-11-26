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

loadDefaults()
group<-c("Total")

rTList <- file2ReportingTriangle(euromomoCntrl)
rTList$rT
rTList$cumRT
rTDF <- rT2DataFrame(rTList$cumRT)
colnames(rTDF)
rownames(rTDF)
