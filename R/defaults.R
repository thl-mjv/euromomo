#' Function to parse a specification file for setting the
#' R-EuroMOMO algorithm parameters. The parser is handwritten based
#' on regular expressions. This is error prone. Future versions could
#' be, e.g., XML based, but this works for now. Based on the philosophy
#' of a SINGLE parameter file, we removed any previous default configurations,
#' but this would be easy to add again (e.g. one defining the 4 STANDARD age groups.)
#'
#'@param fileName of the parameter configuration file
#'@param debug if true print extensive information
#'@export

parseDefaultsFile <- function(fileName=NULL, debug=FALSE) {
  #Here there would be the possibility to add extra files
  #containing, e.g. default parameter configrations
  #defaultFile <- NULL #list.files(patt="^defaults-.*[.]txt$")

#   defaultFile <- system.file("extdata", "defaults.txt", package="euromomo")

  #### Temporary default file - TO BE DELETED- ####
    # Load library
  defaultFile <- file.path(paste(getwd(), "/inst/extdata", sep=""), "defaults.txt")
  #################################################

  files <- c(defaultFile, fileName)

  if(length(files)==0) stop("No parameter configuration file found.")
#   is.null(fileName)
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

#' Function to check, if the currently list stored in options("euromomo")
#' is semantically valid. At the moment, this check consists of:
#' 1. Check for all entries in 'exception' that dStart <= dEnd
#' 2. Each group has a 'definition' and a 'label' attribute.
#' 3. That all Boolean Attributes (e.g. 'trend' and 'seasonality') are really Booleans.
#' At the first error the function stops.
#'
#' @return TRUE, if functions passes finds no errors.
checkOptions <- function() {
  #Extract from global options
  opts <- getOption("euromomo")

  #Check that all important variables are there
  importantVarNames <- c("Country",
                         "Counties",
                         "Institution",
                         "WorkDirectory",
                         "InputFile",
                         "HolidayFile",
                         "BaselineSeasons",
                         "StartDelayEst")

  idxMissing <- which(!(importantVarNames %in% names(opts)))
  if (length(idxMissing)>0) {
    stop("The following variable names are missing: ",importantVarNames[idxMissing],"\n.")
  }


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
    importantAttr <- c("definition","label", "back")
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

  #Check optional parameters of type from:to (where both from and to are integers)
  fromToVarNames <- c("spring","autumn")
  for (i in 1:length(fromToVarNames)) {
    if (fromToVarNames[i] %in% names(opts)) {
      #Check that in format from:to
      fromto <- strsplit( opts[[fromToVarNames[i]]], ":")[[1]]
      if (any(is.na(as.numeric(fromto)))) {
        stop("Definition of \"",fromToVarNames[i],"\" is not in the format from:to.\n")
      }
    }
  }

  #If we get here there were no errors.
  invisible(TRUE)
}

doIt <- function() {
  source("defaults.R")
  parseDefaultsFile("../defaults-example.txt")
  checkOptions()
  #Extract stored list
  opts <- getOption("euromomo")
  opts
}
