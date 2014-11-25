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
#   files0<-system.file("extdata","defaults.txt",package="euromomo")

  files<-list.files(patt="^defaults-.*[.]txt$")
  if(length(files)==0) warning("No defaults files found, using package defaults")
  if(debug) cat("Using these files: ",paste(files,collapse=", "),"\n")
  dats<-unlist(sapply(files,readLines))
  dats<-dats[!grepl("^#",dats)] # remove comments
  dats<-dats[nchar(dats)>0]
  if(debug) print(head(dats))
  #splits<-strsplit(dats,"=")
  # Split on the first equal sign
  splits <- regmatches(dats, regexpr("=",dats), invert=TRUE)

#   labels<-sapply(splits,function(a) strsplit(a[1],"[.]")[[1]])
#   values<-sapply(splits,function(a) a[2])
#   optmat<-t(rbind(labels,values))


  out<-list(exception=list(), groups=list())

for(i in 1:length(splits)) {

    if(!grepl("^(group\\.|except$)", splits[[i]][1])){
      label <- splits[[i]][1]
      value <- splits[[i]][2]
      out[[label]] <- value
    } else {
      if(grepl("^group\\.", splits[[i]][1])){
        group <- strsplit(splits[[i]],"[.]")[[1]][2:3]
        out$groups[[group[1]]][[group[2]]] <- splits[[i]][2]
      } else{

        StartEnd <- c(strsplit(splits[[i]][2], ":"))
        out$exception <- rbind(out$exception, StartEnd[[1]])
      }

    }

  }

  options(euromomo=out)
  invisible(out)
}
