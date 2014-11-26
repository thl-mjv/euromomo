#' Function to parse the group definitions and put in a column for each age group.
#' These are evaluated within the data.frame.
#' 
#' @param momo A data.frame containing the data set.  
#' @return The modified data.frame
#' @export

makeGroups <- function(momo) {
  #Extract the groups from the stored list
  groups <- getOption("euromomo")[["groups"]]
  
  #Loop over all groups. And for each group make it
  for (i in 1:length(groups)) {
    definitionStr <- groups[[i]][["definition"]]
    parseStr <- paste("with(momo, ",definitionStr, ")")
    nameStr <- paste("group_",names(groups)[i],sep="")
    
    #Warning: The definitionStr is evaluated. There is a huge potential for abuse here!
    if (grepl("system\\(|file\\.",definitionStr)) {
      stop("No calling of \"system\" and \"file.*\" is allowed.")    
    }
    
    cat(paste("Creating age group \"",nameStr,"\" based on the expression: ",definitionStr,"\n",sep=""))
    momo[,nameStr] <- eval(parse(text=parseStr))
  }
  
  #Done
  return(momo)
}