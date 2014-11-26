# Load packages
library("ISOweek")
library("foreign")

#' R function for delay corretion based on negative binomial distribution
#' @param data is a dataframe with aggregated number of reported deaths, baseline, Zscores
#' @param group supplies group name
#' @param plot.options selects for output graph type, default is none
#' @value no value as yet
#' @export

diagostic.plots <- function(data, group=NULL, plot.options=("matrix","singles","none")) {
#supply data for now
  data <- diagsdata

#do histograms
#data3 contains final output from baseline function


#plothist makes histograms
plothist <- function(x, plotline=FALSE) {
  h<-hist(x, breaks=10, col="#CAC27E", xlab="", main="", ylab="")
  if(plotline){
    xfit<-seq(min(x),max(x),length=40)
    yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
    yfit <- yfit*diff(h$mids[1:2])*length(x)
    lines(xfit, yfit, col="blue", lwd=2)
  }
}

#plotscatter makes xy plots with lines
plotscatter <- function(x ,y ,plotline=TRUE){
  plot(x, y, ylab="", xlab="", col="blue", bg="blue", pch=16)
  if(plotline){
    z <- lm(y ~ x)
    abline(z, col="darkred", lwd=2)
  }
}



#hist for corrected number of deaths
plothist(data$cnb)
#hist for data used
plothist(data$cnb[data$CondSeason == 1])
#hist for residuals for data used
plothist((data$cnb - data$pnb)[data$CondSeason == 1],TRUE)
#hist for zscores
plothist(data$Zscore, TRUE)


#corrected number of deaths v baseline points
plotscatter(data$pnb, data$cnb)
#data used v baseline points
plotscatter(data$pnb[data$CondSeason == 1], data$cnb[data$CondSeason == 1])
#residuals v baseline points
plotscatter(data$pnb[data$CondSeason == 1], (data$cnb - data$pnb)[data$CondSeason == 1])
