
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
#' @param seasonality Should seasonality be included?
#' @return EuroMOMO data with predicted values, prediction variances and overdispersion
#' @export
baseline <- function(data, seasonality =TRUE, ...){
  # STEP 1: Calculate the trend (YWoDi as continuous)
  data<-addweeks(data)

  # STEP 2: Calculate the sin-cos trends
  data$sin1 <- sin(2*pi * data$wk/52.18)
  data$cos1 <- cos(2*pi * data$wk/52.18)

  # STEP 3: Fit the Poisson model (linear trend) only when the conditions apply
  # Create dummy variable combining all conditions
  data$cond <- with(data,ifelse(Cond3==1 & Cond4==1 & Cond5==1 & Cond6==1, 1, 0))
  ### See the number of deaths on the period of interest. If too few or none, fitting is not possible

  glm_form <- "cnb ~wk"
  if(seasonality==TRUE) glm_form<-paste(glm_form,"+ sin1 + cos1")

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
#' @param spring: Week numbers for spring period
#' @param autumn: Week numbers for autumn period
#' @param duration: Duration of the baseline
#' @param last: The last period that will be excluded
#' @param delay: the number of delay week
#' @return EuroMOMO data with extra variables with conditions used for modelling
#' @export
addconditions <- function(data, spring=15:26, autumn=36:45, duration=5*52, last=NULL, delay=0){

  # Run addweeks function to create some week variables and trend
  data<-addweeks(data)

  # Create Cond3: Period for spring and autumn
  data$Cond3 <- ifelse(with(data, WoDi %in% c(spring, autumn)), 1, 0)

  # Create Cond4: Removing past few months
  if(is.null(last))
    data$Cond4 <- rep(1,nrow(data))

  # Create Cond5: Long delays
  data$Cond5<-with(data,ifelse(wk<max(wk)-delay,1,0))

  # Create Cond6: Length of the baseline (5 years)
  data$Cond6 <- with(data,ifelse(wk > max(wk)-duration - delay, 1, 0))

  return(data)

}


#' Create week variable and trend variable
#' @param data input data in EuroMOMO format
#' @return EuroMOMO data with extra variables with YearWeek and trend variable
#' @export
addweeks<-function(data){

  # Create Year-Week of Death
  data$YWoDi<-with(data,sprintf("%04d-%02d",YoDi,WoDi))

  # Sort the data
  data <- data[order(data$YWoDi), ]

  # Create trend variable
  data$wk <- c(1:nrow(data))
  return(data)
}

#' Append zscores to the EuroMoMo data
#'
#' @param data a data frame in EuroMoMo format
#' @value a data frame in EuroMoMo format
#' @export
zscore <- function(data) {
  data$Zscore <-  with(data,(cnb^(2/3) - pnb^(2/3)) / ((4/9)*(pnb^(1/3))*(overdispersion+pnb*(v.pnb)))^(1/2))

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
excess<-function(data,multiplier=2){
  data$u.pnb<-with(data,(pnb^(2/3)+ multiplier*((4/9)*(pnb^(1/3))*(overdispersion+(v.pnb)*(pnb)))^(1/2))^(3/2) )
  data$l.pnb<-with(data,pmax(0,pnb^(2/3)- multiplier*((4/9)*(pnb^(1/3))*(overdispersion+(v.pnb)*(pnb)))^(1/2))^(3/2) )
  data$excess<-with(data,cnb-pnb)
  data$u.excess<-with(data,cnb-u.pnb)
  data$l.excess<-with(data,cnb-l.pnb)
  return(data)
}

