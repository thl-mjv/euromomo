#' Load defaults for parameters
#'
#' Loads various defaults files and stores them globally.
#' Simplifies the code as all country specific issues can be resolved in these files.
#' All files with pattern defaults-*.dat will be used in alphapetical order.
#' Thus the latter files override the former. So, using defaults-global.dat and
#' defaults-local.dat the latter will override the former.
#' Also, you can use either .dat or .txt so that .txt overrides .dat
#'
#' There is also a default default in the package. It just states that the source files should
#' be downloaded to a subdirectory "download" in the current directory
#'
#'@param debug if true print extensive information
#'@export
loadDefaults<-function(debug=FALSE) {
  files0<-system.file("extdata","defaults.txt",package="euromomo")
  files1<-list.files(patt="^defaults-.*[.]dat$")
  files2<-list.files(patt="^defaults-.*[.]txt$")
  files<-c(files0,sort(files1),sort(files2))
  if(length(files)==0) warning("No defaults files found, using package defaults")
  if(debug) cat("Using these files: ",paste(files,collapse=", "),"\n")
  dats<-unlist(sapply(files,readLines))
  dats<-dats[!grepl("^#",dats)] # remove comments
  dats<-dats[nchar(dats)>0]
  if(debug) print(head(dats))
  splits<-strsplit(dats,"=")
  labels<-sapply(splits,function(a) strsplit(a[1],"[.]")[[1]])
  values<-sapply(splits,function(a) a[2])
  optmat<-t(rbind(labels,values))
  out<-list()
  for(i in 1:nrow(optmat)) {
    country<-optmat[i,1]
    chapter<-optmat[i,2]
    option <-optmat[i,3]
    if(!country%in%names(out))
      out[[country]]<-list()
    if(!chapter%in%names(out[[country]]))
      out[[country]][[chapter]]<-list()
    out[[country]][[chapter]][[option]]<-optmat[i,4]
  }
  options(euromomo=out)
  invisible(out)
}
