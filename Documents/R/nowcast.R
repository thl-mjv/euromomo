#####################################################################
# Test script to compare delay adjustment with the one made in
# Hoehle and an der Heiden (2014), Biometrics, See:
# http://people.su.se/~mhh/pubs/hoehle_anderheiden2014-preprint.pdf
# This functionality is implemented in the R package surveillance
# as function 'nowcast'.
####################################################################

#Load our package.
library("euromomo")

#Load surveillance package.
library("surveillance")

#Read options using the example data.
parseDefaultsFile("defaults.txt")
checkOptions()

# Read in the raw data
momoFile <- readmomofile()

#===========================================================
# Modify Day of Aggregation for reading all data
optsAll <- optsBackup <- getOption("euromomo")
optsAll[["DayOfAggregation"]] <- Sys.Date()
optsAll[["BaselineSeasons"]] <- 7
options("euromomo"=optsAll)
#Read data with this date
momoFileAll <- readmomofile()
#Restore old options.
options("euromomo"=optsBackup)
#====================================================

### read holidays
holiday.file<-holiday(holiday.filename=getOption("euromomo")$HolidayFile)

#Create the groups (as stored in the option file)
momo <- makeGroups(momoFile$momo)
momoAll <- makeGroups(momoFileAll$momo)

### The groups are determined from the options
groups<-names(getOption("euromomo")[["groups"]])

### Make a PDF with all 4+1 default age groups
pdf(file="delayadjustments.pdf",width=8,height=5)
par(mfrow=c(2,3))

#Do comparsion for one group (Here it's the 0-4)
for (i in groups) {
  #i<-"momodefault1"
  groupOpts <- getOption("euromomo")[["groups"]][[i]]
  groupLabel <- groupOpts["label"]
  cat("Group",groupOpts["label"],"\n")

  groupIndicator <- momo[, paste("group_",i,sep="")]
  back<-as.numeric(groupOpts["back"])
  momoGroup <- subset(momo,groupIndicator)
  momoGroupAll <- subset(momoAll,groupIndicator)

  #Get reporting triangle
  rTList <- df2ReportingTriangle(momo, groupIndicator, back, dWeeks=momoFile$dWeeks, dLastFullWeek=momoFile$dLastFullWeek) # something about the group
  rTDF <- rT2DataFrame(rTList$cumRT)

  #Make sts object with the same information.
  #Problem: sts and stsAll do not agree, even on observations far back
  #in time. WHY?
  sts <- linelist2sts( momoGroup, dateCol="DoD", aggregate.by="1 week",
                       dRange=momoFile$dWeeks)
  stsAll <- linelist2sts( momoGroupAll, dateCol="DoD", aggregate.by="1 week",
                       dRange=momoFileAll$dWeeks)

  #Can plot it, if we want to.
  plotPeriod <- nWeeksFromNow(10)
  whichPlot <- epoch(sts) %in% plotPeriod

  if (FALSE) {
    #Show data as weekly data. To understand the plotting symbols check strptime and
    plot(sts[whichPlot,],xaxis.tickFreq=list("%m"=atChange,"%Y"=atChange),
         xaxis.labelFreq=list("%G"=atChange),
         xaxis.labelFormat="%G-W%V",legend.opts=NULL,
         xlab="",las=1,cex.axis=0.8,main=groupLabel, )

    lines(seq_len(length(plotPeriod)),observed(stsAll[whichPlot,]))
  }
  #Small helper function.
  nWeeksFromNow <- function(nWeeks) {
    sort(seq(momoFile$dLastFullWeek,by="-1 week",length.out=nWeeks))
  }



  #Prior with a large variance
  prior <- structure("poisgamma",mean.lambda=mean(observed(sts)),var.lambda=5*var(observed(sts)))
  #noWeeks between dLastFullWeek and StartDelayEst
  nWeeksDelayEst <- as.numeric((momoFile$dLastFullWeek - ISOweek2date(paste(getOption("euromomo")[["StartDelayEst"]],"-1",sep=""))) / 7)

  #Call nowcast function from R package surveillance.
  nc <- nowcast(now=momoFile$dLastFullWeek,when=nWeeksFromNow(back+1),
                data=momoGroup,dEventCol="DoD",dReportCol="DoR",
                method=c("bayes.trunc"), #,"bayes.notrunc.bnb","lawless","bayes.trunc"),
                aggregate.by="1 week",
                D=back,
                m=nWeeksDelayEst, #Moving window for the delay estimation. NULL=take all ATM it takes ALL.
                control=list(dRange=plotPeriod,N.tInf.max=max(observed(sts))*3,N.tInf.prior=prior))

  # Delay adjustment using the momo algorithm.
  drTDF <- delay.nb(rTDF,holiday=holiday.file)

  #Show time series and posterior median forecast/nowcast
  plot(nc,xaxis.tickFreq=list("%d"=atChange,"%m"=atChange),
       xaxis.labelFreq=list("%d"=at2ndChange),
       xaxis.labelFormat="%G-W%V",
       xlab="Time (days)",ylab="No.",
       col=c(NA,"gray","blue"),lty=c(1,1,1,1),lwd=c(1,1,3),legend.opts=NULL,
       main=groupLabel, ylim=c(0,max(observed(stsAll[whichPlot,]),nc@pi,na.rm=TRUE)))

  # Which index to show of drTDF
  idxShow <- which(drTDF$ISOweek %in% ISOweek(plotPeriod))
  lines(seq_len(length(idxShow)), drTDF[idxShow,"cnb"],lwd=3,type="b")

  lines(seq_len(length(idxShow)), drTDF[idxShow,"cnb"] - 1.96*sqrt(drTDF[idxShow,"v.cnb"]),lwd=1,lty=2)
  lines(seq_len(length(idxShow)), drTDF[idxShow,"cnb"] + 1.96*sqrt(drTDF[idxShow,"v.cnb"]),lwd=1,lty=2)


  #Add the truth as well
  #lines(seq_len(length(idxShow))-0.5, observed(stsAll[whichPlot,]),lwd=3,type="s",col="magenta")
}

plot(c(0,0),type="n",axes=FALSE,xlab="",ylab="")
legend(x="center",c("bayes.trunc","delay.nb"),col=c("blue","black"),lwd=3,bg="white",lty=c(1,1))
#legend(x="center",c("bayes.trunc","delay.nb","truth"),col=c("blue","black","magenta"),lwd=3,bg="white",lty=c(1,1,1))

#Close graphics device.
dev.off()

