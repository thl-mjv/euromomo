# Load packages
library(ISOweek)
library(reshape2)
library(foreign)

#' R function for delay corretion based on negative binomial distribution
#' Input: dataframe with aggregated number of reported deaths
#'        string containing the filename of the holiday data
#' Output: dataframe with delay expected number of deaths and its variance

delay.nb <- function(rTDF.cum, holidays.filename) {
  # temporal assingment of objects
  rTDF.cum <- rTDF
  holidays.filename <- "data/IEH3.dta"

  # Read holiday data (assuming it is a stata file, to be changed in the future)
  holiday.data <- read.dta(file = holidays.filename)

  # Add ISOweek tot holiday.data
  holiday.data <- within(holiday.data, {
    ISOweek <- ISOweek(date = date)
  })

  # Aggregate holiday data by ISOweek
  holiday.data.agg <- aggregate(closed ~ ISOweek, data = holiday.data, FUN = sum)

  # Make the holiday triangle
  # First get the isoweeks form rTDF.cum
  hTDF <- data.frame(ISOweek = rTDF.cum$ISOweek)
  # Merge this with the aggregated holidays
  hTDF <- merge(hTDF, holiday.data.agg, all.x = TRUE)
  # Replace NA by 0
  hTDF <- within(hTDF, closed <- ifelse(is.na(closed), 0, closed))

  # Convert closed days to open days and add standard working days
  hTDF <- within(hTDF, {
    open <- euromomoCntrl$nWorkdays - closed
    rm(closed)
  })
  # Add shifted vector with working days to hTDF for the number of delays
  for (i in 1:euromomoCntrl$back) {
    hTDF <- cbind(hTDF, c(rep(NA, i), hTDF$open[1:(nrow(hTDF)-i)]))
  }
  colnames(hTDF) <- c("ISOweek", paste0("open", formatC(0:euromomoCntrl$back, width = 2, flag = "0")))
  # Calculate cumulative number of open days
  hTDF.cum <- cbind(ISOweek = hTDF$ISOweek, as.data.frame(t(apply(hTDF[, -1], MARGIN = 1, FUN = cumsum))))

  # Calculate holiday correction weights (=1 if there are no holidays)
  hTDF.weight <- outer(rep(5, nrow(hTDF.cum)), 1:(euromomoCntrl$back+1)) / hTDF.cum[, -1]

  # Multiply the number of registred deaths by the holiday weights
  rTDF.adjusted <- rTDF.cum[, -1]*hTDF.weight

  # Calculate the fraction reported and its standard error
  fD <- colSums(na.omit(rTDF.adjusted))/sum(na.omit(rTDF.adjusted)[, euromomoCntrl$back+1])
  fD.se <- sqrt(fD*(1-fD)/sum(na.omit(rTDF.adjusted)[, euromomoCntrl$back+1]))

  # Calculate the expected number of deaths using the negative binomial distribution
  # First get the indices of the last known number of deaths of the reporting triangle
  rows <- nrow(rTDF.cum) : (nrow(rTDF.cum)-euromomoCntrl$back)
  cols <- 2:ncol(rTDF.cum)
  # Get the last known number of deaths
  nD <- diag(as.matrix(rTDF.cum[rows, cols]))

  # Make new dataframe for export
  rTDF.pred <- data.frame(
    ISOweek = rTDF.cum$ISOweek,
    cnb = rTDF.cum[, ncol(rTDF.cum)],
    v.cnb = 0)

  # Calculate the expected number of deaths and the variance
  # and plug them into rTDF.pred
  rTDF.pred[rows, "cnb"] <- nD + (nD+1)*(1-fD)/fD
  rTDF.pred[rows, "v.cnb"] <- (nD+1)*(1-fD)/fD^2

  # Export the results
  return(rTDF.pred)
}
