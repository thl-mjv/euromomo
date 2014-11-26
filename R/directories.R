#' Create the needed directories
#'
#' @param wd if "lower", change directory to weekly outputs directory, if "upper" change directory aboe that
#' @param debugmode if TRUE use for debugging
#' @value name of the working subdirectory
#' @export
directories<-function(wd=c("upper","lower"),debugmode=FALSE) {
  opts<-getOption("euromomo")
  if(debugmode)
    root<-tempdir()
  else
    root<-opts$WorkDirectory
  if(!file.exists(root)) stop("Working directory does not exist")

  fi<-try(file.info(root))
  if(inherits(fi,"try-error")) stop("Failed to get information for the working directory")

  if(!fi[1,"isdir"]) stop("Working directory is not a directory")
  week.name<-paste("EUROMOMO",opts$Country,ISOweek(as.Date(opts$DayOfAggregation)),sep="-")
  week.dir<-file.path(root,week.name)
  ok<-dir.create(week.dir)
  if(inherits(ok,"try-error")) stop("Could not create working subdirectory")
  if(match.arg(wd)=="upper")
    setwd(week.dir)
  else
    setwd(root)
  for(i in c("diagnostics","complete"))
    dir.create(file.path(week.dir,i))
  return(week.dir)
}
