

library("ISOweek")
library("foreign")

#do histograms
#data3 contains final output from baseline function


#plothist makes histograms
plothist <- function(x) {
  h<-hist(x, breaks=10, col="#CAC27E", xlab="", main="", ylab="")
  xfit<-seq(min(x),max(x),length=40)
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
  yfit <- yfit*diff(h$mids[1:2])*length(x)
  lines(xfit, yfit, col="blue", lwd=2)
}

#cnb is corrected data series for number of deaths
plothist(data3$cnb)

plothist((data3$cnb - data3$pnb)[data2$CondSeason == 1])

plothist(data2$cnb[data2$CondSeason == 1])
plothist(data4$Zscore)
