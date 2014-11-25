# Load packages
library(ISOweek)
#library(reshape2)
library(foreign)
#install.packages("reshape2")

delay <- function(rTDF, method=c("poisson","negbin"),
                  opentype=c("offset","effect","none"),
                  openvar=c("open","propopen"),
                  delaytype=c("factor","numeric","none"),holidays.filename) {
  # temporal assingment of objects
  holidays.filename <- "data/IEH3.dta"

  # Read holiday data
  holiday.data <- read.dta(file = holidays.filename)

  # Add ISOweek tot holiday.data
  holiday.data <- within(holiday.data, {
    ISOweek <- ISOweek(date = date)
  })

  # Aggregate holiday data by ISOweek
  holiday.data.agg <- aggregate(closed ~ ISOweek, data = holiday.data, FUN = sum)

  # Make the holiday triangle
  # First get the isoweeks form rTDF
  hTDF <- data.frame(ISOweek = rTDF$ISOweek)
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
                     data = rTDF.long, family = poisson, na.action = na.exclude)

  # Predict expected number of reported deaths
  rTDF.pred <- subset(rTDF.long, subset = delay == as.character(euromomoCntrl$back))
  delay.pred<-predict(delay.model, newdata =rTDF.pred, type = "response",se=TRUE)
  rTDF.pred$  cnb <- with(delay.pred,fit)
  rTDF.pred$v.cnb <- with(delay.pred,fit+se.fit^2)
  rTDF.pred<-within(rTDF.pred,{
    u.cnb<-cnb+2*sqrt(v.cnb)
    l.cnb<-cnb-2*sqrt(v.cnb)
  })
  return(rTDF.pred)
}


