#' Load defaults for parameters
#'
#' Loads various defaults files and stores them globally.
#' Simplifies the code as all country specific issues can be resolved in these files.
#' All files with pattern defaults-*.dat will be used in alphapetical order.
#' Thus the latter files override the former. So, using defaults-global.dat and
#' defaults-local.dat the latter will override the former.
#' Also, you can use either .dat or .txt so that .txt overrides .dat
#'
#' There is also a default directory in the package. It just states that the source files should
#' be downloaded to a subdirectory "download" in the current directory
#'
#'@param fileName of the parameter configuration file
#'@param debug if true print extensive information
#'@export
parseDefaultsFile <- function(fileName, debug=FALSE) {
  #Here there would be the possibility to add extra files
  #containing, e.g. default parameter configrations
  #defaultFile <- NULL #list.files(patt="^defaults-.*[.]txt$")  
  files <- fileName
  if(length(files)==0) stop("No parameter configuration file found.")
  if(debug) cat("Using these files: ",paste(files,collapse=", "),"\n")
  
  #Read all files
  dats <- unlist(sapply(files,readLines))
  
  #Strip lines starting with comment symbol and remove empty lines.
  dats <- dats[!grepl("^#",dats)] 
  dats <- dats[nchar(dats)>0]
  if(debug) print(head(dats))
  
  # Split each line on the first equal sign
  splits <- regmatches(dats, regexpr("=",dats), invert=TRUE)

  # Initialize option object containing exception and groups slot.
  out <- list(exception=list(), groups=list())

  # Loop over all remaining lines.
  for(i in 1:length(splits)) {
    #Identifiers starting with 'group.' or 'except' need to be handled specially. 
    if(!grepl("^\\w*(group\\.|except\\w*$)", splits[[i]][1])){
      label <- splits[[i]][1]
      value <- splits[[i]][2]
      out[[label]] <- value
    } else { 
      if (grepl("^\\w*group\\.", splits[[i]][1])){ #group definition?
        #Identify group name and group attribute & add to list
        nameAndAttr <- strsplit(splits[[i]],"[.]")[[1]][2:3]
        out$groups[[nameAndAttr[1]]][[nameAndAttr[2]]] <- splits[[i]][2]
      } else { #except definition
        #Start & End of date range & add to list
        startEnd <- c(strsplit(splits[[i]][2], ":"))
        out$exception <- rbind(out$exception, startEnd[[1]])
      }
    }
  }

  options(euromomo=out)
  invisible(out)
}

checkOptions <- function() {
  #Extract from global options
  opts <- getOption("euromomo")
  
  #Check that ISO weeks of exception are valid.
  dStart <- ISOweek::ISOweek2date(paste(opts$exception[,1],"-1",sep=""))
  dEnd <- ISOweek::ISOweek2date(paste(opts$exception[,2],"-1",sep=""))
  if (any(dStart > dEnd)) {
    idx <- which(dStart > dEnd)
    stop(paste("dStart > dEnd for entries:", paste(opts$exception[idx,],collapse=" : ")))
  }

  #Check that each group has at least the two necessary attributes
  groups <- opts[["groups"]]
  for (i in 1:length(groups)) {
    #Check that the important attributes are there.
    importantAttr <- c("definition","label")
    attrThere <- importantAttr %in% names(groups[[i]])
    if (!all(attrThere)) {
      stop(paste("Group \"",names(groups)[i],"\" is missing the attribute \"",importantAttr[!attrThere],"\"",sep=""))
    }
    #Convert booleans
    booleanAttributes <- c("seasonality","trend")
    for (attr in booleanAttributes) {
      if (attr %in% names(groups[[i]])) {
        if (is.na(as.logical(groups[[i]][[attr]]))) {
          stop(paste("Attribute \"",attr,"\" of group \"",names(groups)[i],"\" is not logical (TRUE/FALSE).",sep=""))
        }
      }
    }
  }
  invisible()
}

doIt <- function() {
  parseDefaultsFile("../defaults-example.txt")
  checkOptions()
}