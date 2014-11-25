# Load packages
library(ISOweek)
library(reshape2)
library(foreign)


delay <- function((rTDF, holidays.filename) {
  # temporal assingment of objects
  holidays.filename <- "../data/IEH3.dta"
  
  # Read holiday data
  holiday.data <- read.dta(file = holidays.filename)
  
  # Add ISOweek tot holiday.data
  holiday.data <- within(holiday.data, {
    ISOweek <- ISOweek(date = date)
  })

  # Aggregate holiday data by ISOweek
  holiday.data.agg <- aggregate(closed ~ ISOweek, data = holiday.data, FUN = sum)
  
  # Get YWoDiFac and DelayFac from cumRT
  YWoDiFac <- rownames(rTList$cumRT)
  DelayFac <- colnames(rTList$cumRT)
  
  
  rTDF <- merge(rTDF, holiday.data.agg, all.x = TRUE)
  rTDF <- within(rTDF, closed <- ifelse(is.na(closed), 0, closed))
  

  


# Read "simulated data"
sim.data <- read.dta(file = "../delay-Total-Ireland-2014-47.dta")

# Reshape sim.data to long format
reshape(data = sim.data, varying = )

# Poisson model for expected reported deaths
glm(deaths ~ YWoDiFac + DelayFac + offset(log(workingdays)), family = poisson)
