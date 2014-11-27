### Start by copying the defaults-example.txt and edit it to suit your needs
if(!file.exists("momomaster.R"))
  if(file.exists("dev"))
    setwd("dev")
getwd()
list.files()
### Example of the workflow in the country
library("euromomo")


### Now using the options
parseDefaultsFile("defaults.txt")
checkOptions()

# Read in the raw data
momoFile <- readmomofile(getOption("euromomo"))

# Create the working directory
week.dir<-directories(lastFullWeek=momoFile$dLastFullWeek)
cat("Results are stored in ",week.dir,"\n")


#Create the groups (as stored in the option file)
momo <- makeGroups(momoFile$momo)

### read holidays HERE
holiday.file<-holiday(holiday.filename=getOption("euromomo")$HolidayFile)

### The groups are determined from the options
groups<-names(getOption("euromomo")[["groups"]])

### This is where the results go
results.list<-list()

for (i in groups) {
  #i<-"Total"
  groupOpts <- getOption("euromomo")[["groups"]][[i]]
  cat("Group",groupOpts["label"],"\n")

  groupIndicator <- momo[, paste("group_",i,sep="")]
  back<-as.numeric(groupOpts["back"])

  rTList <- df2ReportingTriangle(momo, groupIndicator, back, dWeeks=momoFile$dWeeks, dLastFullWeek=momoFile$dLastFullWeek) # something about the group

  rTDF <- rT2DataFrame(rTList$cumRT)
  cat("Group",groupOpts["label"]," reporting triangle\n")
  print(head(rTDF))

  # Delay adjustment
  drTDF<-delay(rTDF,method="negbin",holiday=holiday.file)
  cat("Group",groupOpts["label"]," with delay correction\n")
  print(tail(drTDF,20))

  # Add conditions for the baseline estimation
  data2<-addconditions(drTDF,
                       spring=getOption("euromomo")$spring,
                       autumn=getOption("euromomo")$autumn,
                       delay=back,
                       last=getOption("euromomo")$DayOfAggregation)
  #summary(data2)

  # Estimate the baseline
  data3 <- baseline(data2)
  cat("Group",groupOpts["label"]," with baseline\n")
  print(tail(data3))

  # Calculate Z-scores
  data4 <- zscore(data3)

  # Calculate excess
  data5 <- excess(data4,type="both")
  #tail(data5)

  # Generate output
  try(output(data5))

  # Create diagnostic plots
  try(diagnostic.plots(data5))

  # Store the results
  results.list[[i]]<-data5
}
# on most system, you can check the outputs using
# system(paste("open",week.dir))
final <- do.call("rbind",results.list)
summary(final)
