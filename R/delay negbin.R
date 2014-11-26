# Load packages

#' R function for delay corretion based on negative binomial distribution
#' @param rTDF dataframe with aggregated number of reported deaths
#' @param holiday file with holiday definitions
#' @value a data frame with added column for delay corrected number of deaths and it's estimated variance
#' @export
delay.nb <- function(rTDF, holiday) {
  # options
  opts<-getOption("euromomo")
  # figure out the back parameter
  back<-max(as.numeric(gsub("w","",grep("^w[0-9]*",names(rTDF),value=TRUE))))

  # temporal assingment of objects
  rTDF.cum <- rTDF

  # Make the holiday triangle
  # First get the isoweeks form rTDF.cum
  hTDF <- data.frame(ISOweek = rTDF.cum$ISOweek)
  # Merge this with the aggregated holidays
  hTDF <- merge(hTDF, holiday, all.x = TRUE)
  # Replace NA by 0
  hTDF <- within(hTDF, closed <- ifelse(is.na(closed), 0, closed))

  # Convert closed days to open days and add standard working days
  hTDF <- within(hTDF, {
    # only use the weeks larger than StartDelayEst
    open <- ifelse(as.character(ISOweek)>=opts$StartDelayEst,
                   opts$nWorkdays - closed,
                   NA)
    rm(closed)
  })
  print(summary(hTDF))
  # Add shifted vector with working days to hTDF for the number of delays
  for (i in 1:back) {
    hTDF <- cbind(hTDF, c(rep(NA, i), hTDF$open[1:(nrow(hTDF)-i)]))
  }
  colnames(hTDF) <- c("ISOweek", paste0("open", formatC(0:back, width = 2, flag = "0")))
  # Calculate cumulative number of open days
  hTDF.cum <- cbind(ISOweek = hTDF$ISOweek, as.data.frame(t(apply(hTDF[, -1], MARGIN = 1, FUN = cumsum))))

  # Calculate holiday correction weights (=1 if there are no holidays)
  hTDF.weight <- outer(rep(5, nrow(hTDF.cum)), 1:(back+1)) / hTDF.cum[, -1]

  # Multiply the number of registred deaths by the holiday weights
  rTDF.adjusted <- rTDF.cum[, -1]*hTDF.weight

  # Calculate the fraction reported and its standard error
  fD <- colSums(na.omit(rTDF.adjusted))/sum(na.omit(rTDF.adjusted)[, back+1])
  fD.se <- sqrt(fD*(1-fD)/sum(na.omit(rTDF.adjusted)[, back+1]))

  # Calculate the expected number of deaths using the negative binomial distribution
  # First get the indices of the last known number of deaths of the reporting triangle
  rows <- nrow(rTDF.cum) : (nrow(rTDF.cum)-back)
  cols <- 2:ncol(rTDF.cum)
  # Get the last known number of deaths
  nD <- diag(as.matrix(rTDF.cum[rows, cols]))

  rTDF.last<-apply(as.matrix(rTDF.cum[,-1]),1,max,na.rm=TRUE) # latest observed
  # Make new dataframe for export
  rTDF.pred <- data.frame(
    ISOweek = rTDF.cum$ISOweek,       # Week
    nb = rTDF.cum[, ncol(rTDF.cum)],  # observed full
    onb = rTDF.last,                  # latest registered
    cnb = rTDF.cum[, ncol(rTDF.cum)], # to be filled with expected
    v.cnb = 0) # to be filled with variance

  # Calculate the expected number of deaths and the variance
  # and plug them into rTDF.pred
  rTDF.pred[rows, "cnb"] <- nD + (nD+1)*(1-fD)/fD
  rTDF.pred[rows, "v.cnb"] <- (nD+1)*(1-fD)/fD^2

  # Export the results
  return(rTDF.pred)
}
