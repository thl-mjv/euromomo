#' Manage the holiday files
#'
#' @param holiday.filename name of the file
#' @value a data.frame with columns ISOweek and something
#' @export
holiday<-function(holiday.filename="data/IEH3.dta") {
  # Read holiday data
  if(grepl("[.]dta$",holiday.filename)) {
    cat("Assuming the file is in Stata format")
    require("foreign")
    holiday.data <- read.dta(file = holidays.filename)
  }
  # Add ISOweek tot holiday.data
  holiday.data <- within(holiday.data, {
    ISOweek <- ISOweek(date = date)
  })

  # Aggregate holiday data by ISOweek
  aggregate(closed ~ ISOweek, data = holiday.data, FUN = sum)

}
#' Delay correction
#'
#' @param rTDF source data from aggregation
#' @param method either "poisson" or "negbin"
#' @param opentype which type of effect for the "open"?
#' @param openvar which version of the variable "open" to user=
#' @param delaytype which kind of effect for the delay variable?
#' @param holiday file with holiday definitions
#' @value a data frame with added column for delay corrected number of deaths and it's estimated variance
#' @export
delay <- function(rTDF,
                  method=c("poisson","negbin"),
                  opentype=c("offset","effect","none"),
                  openvar=c("open","propopen"),
                  delaytype=c("factor","numeric","none"),holiday) {
  # temporal assingment of objects
  # Make the holiday triangle
  # First get the isoweeks form rTDF
  hTDF <- data.frame(ISOweek = rTDF$ISOweek)
  # Merge this with the aggregated holidays
  hTDF <- merge(hTDF, holiday, all.x = TRUE)
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
  # Calculate cumulative open days
  cumHT <- cbind(ISOweek = hTDF$ISOweek, as.data.frame(t(apply(hTDF[, -1], MARGIN = 1, FUN = cumsum))))

  # Reshape rTDF into long format
  rTDF.long <- reshape(
    data = rTDF,
    varying = names(rTDF)[-1],
    v.names = "wr",
    timevar = "delay",
    times = factor(0:euromomoCntrl$back),
    idvar = "ISOweek",
    direction = "long")

  # Reshape cumHT into long format
  cumHT.long <- reshape(
    data = cumHT,
    varying = names(cumHT)[-1],
    v.names = "open",
    timevar = "delay",
    times = factor(0:euromomoCntrl$back),
    idvar = "ISOweek",
    direction = "long")

  # Merge rTDF.long with cumHT.long
  rTDF.long <- merge(rTDF.long, cumHT.long)
  rTDF.long$onb<-with(rTDF.long,ave(na.0(wr),ISOweek,FUN=max))
  rTDF.long$numdelay<-as.numeric(rTDF.long$delay)

  rTDF.long$maxopen<-with(rTDF.long,ave(open,ISOweek,FUN=max))
  rTDF.long$propopen<-with(rTDF.long,open/maxopen)
  # Poisson model for expected reported deaths
  ## offset: none, open, open/maxopen, ...
  ## delay: factor/numeric/...
  delay.formula<-"wr ~ ISOweek"
  delaytype<-match.arg(delaytype)
  if(delaytype=="factor") delay.formula<-paste(delay.formula,"+delay")
  if(delaytype=="numeric") delay.formula<-paste(delay.formula,"+numdelay")

  opentype<-match.arg(opentype)
  openvar<-match.arg(openvar)
  if(opentype=="offset") delay.formula<-paste(delay.formula,"+offset(log(",openvar,"))",sep="")
  if(opentype=="effect") delay.formula<-paste(delay.formula,"+log(",openvar,")",sep="")

  print(delay.formula)
  delay.model <- glm(formula(delay.formula),
                     data = rTDF.long, family = quasipoisson(), na.action = na.exclude)

  # Predict expected number of reported deaths
  rTDF.pred <- subset(rTDF.long, subset = delay == as.character(euromomoCntrl$back))
  delay.pred<-predict(delay.model, newdata =rTDF.pred, type = "response",se=TRUE)
  rTDF.pred$ p.cnb <- with(delay.pred,fit)
  rTDF.pred$vp.cnb <- with(delay.pred,se.fit^2)
  rTDF.pred$vv.cnb <- with(delay.pred,fit+se.fit^2)
  rTDF.pred$   cnb <- with(rTDF.pred,ifelse(is.na(wr),p.cnb,wr))
  rTDF.pred$ v.cnb <- with(rTDF.pred,ifelse(is.na(wr),vp.cnb,0))

  return(rTDF.pred)
}


