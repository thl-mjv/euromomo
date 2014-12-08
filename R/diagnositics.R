# Load packages

#' R function plotting histograms and scatter plots of data, residuals and Z-scores
#' @param data is a dataframe with aggregated number of reported deaths, baseline, Zscores
#' @param plot.options selects for output graph type, default is matrix
#' @export

diagnostic.plots <- function(data, plot.options=c("matrix", "singles", "none")) {
  # set matrix style output as default
  plot.options <- match.arg(plot.options)


  #saving data to subfolder
  #check subfolder exists ... to do

  if(plot.options=="singles"){

    #hist for corrected number of deaths
    filename <- paste0("diagnostics/Hist-Deaths-", groupOpts["label"], ".png")
    png(filename = file.path(week.dir, filename))
    plothist(data$cnb, title=paste("All Data", groupOpts["label"], sep="-"))
    dev.off()

    #hist for data used
    filename <- paste0("diagnostics/Hist-UsedData-", groupOpts["label"], ".png")
    png(filename = file.path(week.dir, filename))
    plothist(data$cnb[data$cond == 1], title=paste("Used Data",groupOpts["label"], sep="-"))
    dev.off()

    #hist for residuals for data used
    filename <- paste0("diagnostics/Hist-Residuals-",groupOpts["label"], ".png")
    png(filename = file.path(week.dir, filename))
    plothist((data$cnb - data$pnb)[data$cond == 1],plotline=TRUE,
             title=paste("Residuals of Used Data", groupOpts["label"], sep="-"))
    dev.off()

    #hist for zscores
    filename <- paste0("diagnostics/Hist-Zscores-",groupOpts["label"], ".png")
    png(filename = file.path(week.dir, filename))
    plothist(data$Zscore, plotline=TRUE, title=paste("Zscores",groupOpts["label"], sep="-"))
    dev.off()


    #corrected number of deaths v baseline points
    filename <- paste0("diagnostics/Scatter-Deaths-", groupOpts["label"], ".png")
    png(filename = file.path(week.dir, filename))
    plotscatter(data$pnb, data$cnb, title=paste("All Data", groupOpts["label"], sep="-"))
    dev.off()

    #data used v baseline points
    filename <- paste0("diagnostics/Scatter-UsedData-", groupOpts["label"], ".png")
    png(filename = file.path(week.dir, filename))
    plotscatter(data$pnb[data$cond == 1], data$cnb[data$cond == 1],
                title=paste("Used Data", groupOpts["label"], sep="-"))
    dev.off()

    #residuals v baseline points
    filename <- paste0("diagnostics/Scatter-Residuals-", groupOpts["label"], ".png")
    png(filename = file.path(week.dir, filename))
    plotscatter(data$pnb[data$cond == 1], (data$cnb - data$pnb)[data$cond == 1],
                title=paste("Residuals of Used Data", groupOpts["label"], sep="-"))
    dev.off()


    #corrected number of deaths v baseline points
    filename <- paste0("diagnostics/Periodogram-Deaths-", groupOpts["label"], ".png")
    png(filename = file.path(week.dir, filename))
    plotspectrum(data$cnb, title=paste("All Data", groupOpts["label"], sep="-"))
    dev.off()

    #data used v baseline points
    filename <- paste0("diagnostics/Periodogram-UsedData-", groupOpts["label"], ".png")
    png(filename = file.path(week.dir, filename))
    plotspectrum(data$cnb[data$cond == 1],
                 title=paste("Used Data", groupOpts["label"], sep="-"))
    dev.off()

    #residuals v baseline points
    filename <- paste0("diagnostics/Periodogram-Residuals-", groupOpts["label"], ".png")
    png(filename = file.path(week.dir, filename))
    plotspectrum((data$cnb - data$pnb)[data$cond == 1],
                 title=paste("Residuals of Used Data", groupOpts["label"], sep="-"))
    dev.off()

  }


  if(plot.options=="matrix"){
    # Open connection to png file
    filename <- paste0("Diagnostics ", groupOpts["label"], ".png")
    png(filename = file.path(week.dir, "diagnostics", filename), width = 1000, height = 600, units = "px", pointsize = 12)
    par(mfrow = c(3,4))
    #hist for corrected number of deaths
    plothist(data$cnb, title="All Data")
    #hist for data used
    plothist(data$cnb[data$cond == 1], title="Used Data")
    #hist for residuals for data used
    plothist((data$cnb - data$pnb)[data$cond == 1],plotline=TRUE, title="Residuals of Used Data")
    #hist for zscores
    plothist(data$Zscore, plotline=TRUE, title="Zscores")
    #corrected number of deaths v baseline points
    plotscatter(data$pnb, data$cnb, title="Baseline v All Data")
    #data used v baseline points
    plotscatter(data$pnb[data$cond == 1], data$cnb[data$cond == 1],
                title="Baseline v Used Data")
    #residuals v baseline points
    plotscatter(data$pnb[data$cond == 1], (data$cnb - data$pnb)[data$cond == 1],
                title="Residuals v Used Data")
    #plot blank for the fourth slot
    plot.new()
    #corrected number of deaths v baseline points
    plotspectrum(data$cnb, title="All Data")
    #data used v baseline poin
    plotspectrum(data$cnb[data$cond == 1], title="Used Data")
    #residuals v baseline points
    plotspectrum((data$cnb - data$pnb)[data$cond == 1], title="Residuals of Used Data")
    #plot blank for the fourth slot
    plot.new()
    dev.off()

    par(mfrow=c(1,1))
  }


}

#' function for plotting histograms, called from within the main diagnostic.plots
#' @param x A vector of values for the histogram
#' @param plotline TRUE or FALSE (default) for plotting normal fit line
#' @param title to pass as the title in a plot
#' @export
plothist <- function(x, plotline=FALSE, title=NULL) {
  h<-hist(x, breaks=10, col="#CAC27E", xlab="", main=title, ylab="")
  if(plotline){
    xfit <- seq(min(x), max(x), length=40)
    yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
    yfit <- yfit*diff(h$mids[1:2])*length(x)
    lines(xfit, yfit, col="blue", lwd=2)
  }
}


#' function for scatter plots, called from within the main diagnostic.plots
#' @param x A x-axis values for the scatter plot
#' @param y A y-axis values for the scatter plot
#' @param plotline TRUE (default) or FALSE for plotting slr fit line
#' @param title to pass as the title in a plot
#' @export
plotscatter <- function(x ,y ,plotline=TRUE, title=NULL){
  plot(x, y, ylab="", xlab="", col="blue", bg="blue", pch=16, main=title)
  if(plotline){
    z <- lm(y ~ x)
    abline(z, col="darkred", lwd=2)
  }
}


#' function for plotting periodograms (spectral densities on the log scale), called from within the main diagnostic.plots
#' @param x A vector of values from a time series
#' @param plotline TRUE (default) or FALSE for plotting vertical lines
#' @param title to pass as the title in a plot
#' @export
plotspectrum <- function(x, plotline=TRUE, title=NULL) {
  with(spectrum(x, plot=FALSE),
       plot(1/freq-1, spec, log="xy", type="l",
            xlab = "Period", ylab = "", main=title))
  if(plotline){
    abline(v=c(365.25/28, 365.25/14, 365.25/7), col="blue")
  }
}

