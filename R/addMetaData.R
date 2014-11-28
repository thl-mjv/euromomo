#' Function to add meta data.
#'
#' @param df A \code{data.frame} containing the model output.
#' @param groupName A string containing the name of the group.
#' @param groupOptions A vector of strings containing the group specific options.
#' @note There is a lot of redudant data here!
#' @export
addMetaData <- function(df, groupName, groupOptions) {
  #Extract options
  opts <- getOption("euromomo")

  #Define which parameters to extract from option object and extract.
  varNames <- c("Country","Counties","Institution","BaselineSeasons","StartDelayEst",
                "DayOfAggregation","nWorkdays")
  varNamesValues <- as.data.frame(t(sapply(varNames, function(name) opts[[name]])),stringsAsFactors=FALSE)
  #Group specific information.
  groupNameValues <- as.data.frame(t(groupOpts),stringsAsFactor=FALSE)

  #Add as columns to the data.frame
  tmp <- cbind(varNamesValues,group.name=groupName,group=groupNameValues, df)

  #Done.
  return(tmp)
}

# Country=Denmark
# Counties=Seeland,Jylland
# Institution=ISS
# WorkDirectory=.
# InputFile=../data/DoD_DoR.txt
# HolidayFile=../data/holidays.txt
# BaselineSeasons=5
# StartDelayEst=2008-W01
# DayOfAggregation=2013-01-03
# nWorkdays=5
