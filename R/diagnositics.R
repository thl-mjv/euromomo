# Load packages

#' R function for delay corretion based on negative binomial distribution
#' @param data is a dataframe with aggregated number of reported deaths, baseline, Zscores
#' @param plot.options selects for output graph type, default is matrix
#' @value no value as yet
#' @export

diagnostic.plots <- function(data, plot.options=c("matrix", "singles", "none")) {
# set matrix style output as default
  plot.options <- match.arg(plot.options)

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

    #hist for corrected number of deaths
    filename <- paste0("diagnostics/Hist-Deaths-",groupOpts["label"], ".png")
    png(filename = file.path(week.dir, filename))
      plothist(data$cnb, title=paste("Deaths",groupOpts["label"],sep="-"))
    dev.off()

    #hist for data used
    filename <- paste0("diagnostics/Hist-UsedData-",groupOpts["label"], ".png")
    png(filename = file.path(week.dir, filename))
      plothist(data$cnb[data$cond == 1], title=paste("Used Data",groupOpts["label"],sep="-"))
    dev.off()


    #hist for residuals for data used
    filename <- paste0("diagnostics/Hist-Residuals-",groupOpts["label"], ".png")
    png(filename = file.path(week.dir, filename))
      plothist((data$cnb - data$pnb)[data$cond == 1],plotline=TRUE,
             title=paste("Residuals",groupOpts["label"],sep="-"))
    dev.off()

    #hist for zscores
    filename <- paste0("diagnostics/Hist-Zscores-",groupOpts["label"], ".png")
    png(filename = file.path(week.dir, filename))
      plothist(data$Zscore, plotline=TRUE, title=paste("Zscores",groupOpts["label"],sep="-"))
    dev.off()

    #corrected number of deaths v baseline points
    filename <- paste0("diagnostics/Scatter-Deaths-",groupOpts["label"], ".png")
    png(filename = file.path(week.dir, filename))
      plotscatter(data$pnb, data$cnb, title=paste("Deaths",groupOpts["label"],sep="-"))
    dev.off()


    #data used v baseline points
    filename <- paste0("diagnostics/Scatter-UsedData-",groupOpts["label"], ".png")
    png(filename = file.path(week.dir, filename))
      plotscatter(data$pnb[data$cond == 1], data$cnb[data$cond == 1],
                title=paste("Used Data",groupOpts["label"],sep="-"))
    dev.off()

    #residuals v baseline points
    filename <- paste0("diagnostics/Scatter-Residuals-",groupOpts["label"], ".png")
    png(filename = file.path(week.dir, filename))
      plotscatter(data$pnb[data$cond == 1], (data$cnb - data$pnb)[data$cond == 1],
                title=paste("Residuals",groupOpts["label"],sep="-"))
    dev.off()
    }


  if(plot.options=="matrix"){
    # Open connection to png file
    filename <- paste0("Diagnostics ", groupOpts["label"], ".png")
    png(filename = file.path(week.dir, "diagnostics", filename), width = 1000, height = 600, units = "px", pointsize = 12)
    par(mfrow = c(2,4))
    #hist for corrected number of deaths
    plothist(data$cnb, title="Deaths")
    #hist for data used
    plothist(data$cnb[data$cond == 1], title="Used Data")
    #hist for residuals for data used
    plothist((data$cnb - data$pnb)[data$cond == 1],plotline=TRUE, title="Residuals")
    #hist for zscores
    plothist(data$Zscore, plotline=TRUE, title="Zscores")
    #corrected number of deaths v baseline points
    plotscatter(data$pnb, data$cnb, title="Deaths")
    #data used v baseline poin
    plotscatter(data$pnb[data$cond == 1], data$cnb[data$cond == 1],
                title="Used Data")
    #residuals v baseline points
    plotscatter(data$pnb[data$cond == 1], (data$cnb - data$pnb)[data$cond == 1],
                title="Residuals")
    dev.off()
    par(mfrow=c(1,1))
  }



}

