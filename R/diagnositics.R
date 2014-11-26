# Load packages
library("ISOweek")
library("foreign")

#' R function for delay corretion based on negative binomial distribution
#' @param data is a dataframe with aggregated number of reported deaths, baseline, Zscores
#' @param plot.options selects for output graph type, default is none
#' @value no value as yet
#' @export

diagostic.plots <- function(data, plot.options=("matrix","singles","none",...)) {
# Temporal assignment of data
  data <- diagsdata
  # Add additional information (to be added: should be part of data already)
    country <- "England"
    group <- "Total"
    DateoA <- euromomoCntrl$dAggregation
    WoAi <- as.numeric(substr(ISOweek::date2ISOweek(DateoA), start = 7, stop = 8))
    YoAi <- as.numeric(substr(ISOweek::date2ISOweek(DateoA), start = 1, stop = 4))
    current.folder <- "testdiagnostics"
    #do histograms

  #plothist makes histograms
  plothist <- function(x, plotline=FALSE, title=NULL) {
    h<-hist(x, breaks=10, col="#CAC27E", xlab="", main=title, ylab="")
    if(plotline){
      xfit<-seq(min(x),max(x),length=40)
      yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
      yfit <- yfit*diff(h$mids[1:2])*length(x)
      lines(xfit, yfit, col="blue", lwd=2)
    }
  }

  #plotscatter makes xy plots with lines
  plotscatter <- function(x ,y ,plotline=TRUE, title=NULL){
  plot(x, y, ylab="", xlab="", col="blue", bg="blue", pch=16, main=title)
  if(plotline){
    z <- lm(y ~ x)
    abline(z, col="darkred", lwd=2)
    }
  }

  #saving data to subfolder
  #check subfolder exists ... to do

  if(plot.options=="singles"){
    title <- paste(group, YoAi, WoAi, sep="-")

    #hist for corrected number of deaths
    png(file=paste(current.folder, "/Hist-Deaths-",title,".png",sep=""))
      plothist(data$cnb, title=paste("Deaths",title,sep="-"))
    dev.off()

    #hist for data used
    png(file=paste(current.folder, "/Hist-UsedData-",title,".png",sep=""))
      plothist(data$cnb[data$CondSeason == 1], title=paste("Used Data",title,sep="-"))
    dev.off()

    #hist for residuals for data used
    png(file=paste(current.folder, "/Hist-Residuals-",title,".png",sep=""))
      plothist((data$cnb - data$pnb)[data$CondSeason == 1],plotline=TRUE,
             title=paste("Residuals",title,sep="-"))
    dev.off()

    #hist for zscores
    png(file=paste(current.folder, "/Hist-Zscores-",title,".png",sep=""))
      plothist(data$Zscore, plotline=TRUE, title=paste("Zscores",title,sep="-"))
    dev.off()

    #corrected number of deaths v baseline points
    png(file=paste(current.folder, "/Scatter-Deaths-",title,".png",sep=""))
      plotscatter(data$pnb, data$cnb, title=paste("Deaths",title,sep="-"))
    dev.off()

    #data used v baseline points
    png(file=paste(current.folder, "/Scatter-UsedData-",title,".png",sep=""))
      plotscatter(data$pnb[data$CondSeason == 1], data$cnb[data$CondSeason == 1],
                title=paste("Used Data",title,sep="-"))
    dev.off()

    #residuals v baseline points
    png(file=paste(current.folder, "/Scatter-Residuals-",title,".png",sep=""))
      plotscatter(data$pnb[data$CondSeason == 1], (data$cnb - data$pnb)[data$CondSeason == 1],
                title=paste("Residuals",title,sep="-"))
    dev.off()
    }


  if(plot.options=="matrix"){
    title <- paste(group, YoAi, WoAi, sep="-")
    png(file=paste(current.folder, "/Diagnostics-",title,".png",sep=""), width = 800)
    par(mfrow = c(2,4))
    #hist for corrected number of deaths
    plothist(data$cnb, title=paste("Deaths"))
    #hist for data used
    plothist(data$cnb[data$CondSeason == 1], title=paste("Used Data"))
    #hist for residuals for data used
    plothist((data$cnb - data$pnb)[data$CondSeason == 1],plotline=TRUE, title=paste("Residuals"))
    #hist for zscores
    plothist(data$Zscore, plotline=TRUE, title=paste("Zscores"))
    #corrected number of deaths v baseline points
    plotscatter(data$pnb, data$cnb, title=paste("Deaths"))
    #data used v baseline points
    plotscatter(data$pnb[data$CondSeason == 1], data$cnb[data$CondSeason == 1],
                title=paste("Used Data"))
    #residuals v baseline points
    plotscatter(data$pnb[data$CondSeason == 1], (data$cnb - data$pnb)[data$CondSeason == 1],
                title=paste("Residuals"))
    dev.off()
  }

}
