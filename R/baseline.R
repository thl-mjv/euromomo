
# We assume the data come in the following format
#
# Data structure:
#   - cnb : COrrected (delay-adjusted) number of deaths
#   - YoDi: Year of Deaths - ISO week
#   - WoDi: Weeks of deaths ISO week
#   - YWoDi: Year and Week of Deaths - ISO week
#   - Cond3: Dummy variable for period estimation (summer/autumn/winter/spring)
#   - Cond4: Dummy variable for removing past few months (?)
#   - Cond5: Dummy variable for long delays
#   - Cond6: Dummy variable for the length of the baseline (5 years)
#
#
#
# STEP 1: Calculate the trend (YWoDi as continuous)
# STEP 2: Calculate the sin-cos trends
# STEP 3: Fit the Poisson model (linear trend) only when the conditions apply
# STEP 4: Prediction of the model
#

#' Calculate baseline
#' @param data input data in EuroMOMO format
#' @param seasonality number of seasonality components
#' @param spline type of spline
#' @param splinedf number of degrees of freedom in the spline basis
#' @param group which group to use. Groups are defined using variables so this must be a name of an actual variable in the data
#' @return EuroMOMO data with predicted values, prediction variances and overdispersion
#' @export
baseline <- function(data, seasonality =NULL, spline=c("none","cubic","linear"),
                     splinedf=5,group=NULL,...){
  # STEP 1: Calculate the trend (ISOweek) as continuous)
  data<-addweeks(data,group=group)
  # possible spline basis generation goes here
  spline<-match.arg(spline)
  if(!splinedf%in%1:5) warning("Nonsense degrees of freedom to spline basis")
  if(spline=="cubic") data$basis<-bs(data$wk,df=splinedf)
  if(spline=="linear") data$basis<-bs(data$wk,df=splinedf,degree=1)


  # STEP 2: Calculate the sin-cos trends
  if(is.null(seasonality))
    seasonality<-as.numeric(getOption("euromomo")$all$baseline$seasonality)
  if(seasonality>0) {
    seasonality<-pmax(1,seasonality)[1]
    for(i in 1:seasonality) {
      data[[paste("sin",i,sep="")]] <- sin(i*2 * pi * data$wk/(52.18))
      data[[paste("cos",i,sep="")]] <- cos(i*2 * pi * data$wk/(52.18))
    }
  }
  # STEP 3: Fit the Poisson model (linear trend) only when the conditions apply
  # Create dummy variable combining all conditions
  condition.variables<-grep("^Cond",names(data))
  data$cond<-rep(1,nrow(data))
  for(i in condition.variables) data$cond<-with(data,ifelse(data[[i]]==1 & cond==1,1,0))
  #data$cond <- with(data,ifelse(Cond3==1 & Cond4==1 & Cond5==1 & Cond6==1, 1, 0))
  ### See the number of deaths on the period of interest. If too few or none, fitting is not possible
  active_deaths<-sum(subset(data,cond==1)$cnb)
  if(active_deaths<10) warning("Number of deaths used for baseline estimation is very low")

  glm_form <- "cnb ~wk"
  if(seasonality>0)
    glm_form<-paste(glm_form,"+",paste(as.vector(outer(c("sin","cos"),1:seasonality,paste,sep="")),collapse="+"))
  if(spline%in%c("linear","cubic")) glm_form<-paste(glm_form,"+basis")

  # Need to consider (at some point) whether to allow using other estimation functions (glm2, mgcv, ...)
  fit <- try(glm(formula(glm_form), data = data[data$cond==1, ], family = quasipoisson()))

  if(!inherits(fit,"try-error")) {

  # STEP 4: Prediction of the model
  pred <- predict(fit, newdata = data, type = "link", se=TRUE)

  # Attach fitted values to dataset
  invlink<-family(fit)$linkinv
  data$pnb <- invlink(pred$fit)

  # Attach predistion variance to dataset
  data$v.pnb <- pred$se.fit^2

  # Attach overdispersion values to dataset
  data$overdispersion <- summary(fit)$dispersion
  }

  return(data)
}

#' Create condition variables and add them to the dataset
#' @param data input data in EuroMOMO format
#' @param spring: Week numbers for spring period, vector of integers between 1 and 53
#' @param autumn: Week numbers for autumn period, vector of integers between 1 and 53
#' @param duration: Duration of the baseline
#' @param last: The last period that will be excluded
#' @param delay: the number of delay week
#' @param group which group to use. Groups are defined using variables so this must be a name of an actual variable in the data
#' @return EuroMOMO data with extra variables with conditions used for modelling
#' @export
addconditions <- function(data, spring=15:26, autumn=36:45, duration=5*52, last=NULL, delay=0,group=NULL){

  # Run addweeks function to create some week variables and trend
  data<-addweeks(data,group=group)

  # Create Cond3: Period for spring and autumn
  # spring and autumn must be vectors of integers between 1 and 53
  data$CondSeason <- ifelse(with(data, WoDi %in% c(spring, autumn)), 1, 0)

  # Create Cond4: Removing past few months
  if(is.null(last))
    data$CondSomething <- rep(1,nrow(data))

  # Create Cond5: Long delays
  delay<-pmax(0,delay)[1]
  data$CondDelays<-with(data,ifelse(wk<max(wk)-delay,1,0))

  # Create Cond6: Length of the baseline (5 years)
  duration<-pmax(155,duration)[1]
  data$CondLength <- with(data,ifelse(wk > max(wk) - duration - delay, 1, 0))

  return(data)

}


#' Create week variable and trend variable
#' @param data input data in EuroMOMO format
#' @param group which group to use. Groups are defined using variables so this must be a name of an actual variable in the data
#' @return EuroMOMO data with extra variables with YearWeek and trend variable
#' @export
addweeks<-function(data,group=NULL){
  # Checks
  if(!"ISOweek"%in%names(data)) stop("Invalid data")
  if(!all(c("YoDi","WoDi")%in%names(data))) {
    data$YoDi<-as.numeric(substring(as.character(data$ISOweek),1,4))
    data$WoDi<-as.numeric(substring(as.character(data$ISOweek),7))
  }

  # Subset to right group
  # fails if called as addweeks(data,groups=c("something","somethingelse"))
  if(!is.null(group)) {
    if(length(group)>1) group<-group[1]
    if(!group%in%names(data)) warning("Group variable not found in the data")
    else data<-subset(data,get(group)==1)
  }

  # Create Year-Week of Death - NO LONGER NEEDE
  #if(!"YWoDi"%in%names(data))
  #  data$YWoDi<-with(data,sprintf("%04d-%02d",YoDi,WoDi))

  # Sort the data
  if(!"wk"%in%names(data)) {
    data <- data[order(data$ISOweek), ]
    # Create trend variable
    data$wk <- c(1:nrow(data))
  }
  return(data)
}

#' Append zscores to the EuroMoMo data
#'
#' @param data a data frame in EuroMoMo format
#' @value a data frame in EuroMoMo format
#' @export
zscore <- function(data,type=c("baseline","both")) {
  type<-match.arg(type)
  blvars<-c("pnb","overdispersion","v.pnb")
  # if we requested something needing baseline and it is not available, issue warnings and go away
  if(!all(blvars%in%names(data))&type%in%c("baseline","both")) {
    warning("No baseline found")
    return(data)
  }
  dlvars<-c("cnb","v.cnb")
  # if we requested something needing baseline and it is not available, issue warnings and go away
  if(!all(dlvars%in%names(data))&type%in%c("both")) {
    warning("No baseline found")
    return(data)
  }

  if(type=="baseline")
    data$Zscore <-  with(data,(cnb^(2/3) - pnb^(2/3)) / ((4/9)*(pnb^(1/3))*(overdispersion+pnb*(v.pnb)))^(1/2))
  if(type=="both")
    data$Zscore <- with(data,(cnb^(2/3) - pnb^(2/3)) / ((4/9)*(pnb^(1/3))*(overdispersion+v.cnb/pnb+pnb*(v.pnb)))^(1/2))

  return(data)
}

#' Excess estimation
#'
#' Calculates confidence intervals for the prediction and excess
#'
#' @param data a data frame in EuroMoMo format
#' @param multiplier how many approximate standard deviations to use?
#' @value a data frame in EuroMoMo format
#' @export
excess<-function(data,multiplier=2,type=c("baseline","basedelay","delay","both")){
  type<-match.arg(type)
  blvars<-c("pnb","overdispersion","v.pnb")
  # if we requested something needing baseline and it is not available, issue warnings and go away
  if(!all(blvars%in%names(data))&type%in%c("baseline","both","basedelay")) {
    warning("No baseline found")
    return(data)
  }
  dlvars<-c("cnb","v.cnb")
  # if we requested something needing baseline and it is not available, issue warnings and go away
  if(!all(dlvars%in%names(data))&type%in%c("delay","both","basedelay")) {
    warning("No baseline found")
    return(data)
  }
  if(type%in%c("baseline","both","basedelay")) {
    if(type=="baseline")
      data$pv.pnb<-with(data,((4/9)*(pnb^(1/3))*(overdispersion+(v.pnb)*(pnb)))^(1/2))
    else
      data$pv.pnb<-with(data,((4/9)*(pnb^(1/3))*(overdispersion+(v.cnb)/pnb+(v.pnb)*(pnb)))^(1/2))
    data$u.pnb<-with(data,(pnb^(2/3)+ multiplier*pv.pnb)^(3/2))
    data$l.pnb<-with(data,pmax(0,pnb^(2/3)- multiplier*pv.pnb)^(3/2))
    data$excess<-with(data,cnb-pnb)
    data$u.excess<-with(data,cnb-u.pnb)
    data$l.excess<-with(data,cnb-l.pnb)
  }
  if(type%in%c("delay","both")) {
    # TODO: figure out the 2/3 -power transformation
    data$pv.cnb<-with(data,ifelse(v.cnb==0,0,(v.cnb)^(1/2)))
    data$u.cnb<-with(data,(cnb+multiplier*pv.cnb))
    data$l.cnb<-with(data,(cnb-multiplier*pv.cnb))

  }
  return(data)
}

