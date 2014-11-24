
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
#' @value EuroMOMO data with predicted values, prediction variances and overdispersion
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
  pred <- predict(fit, newdata = data, type = "response", se=TRUE)

  # Attach fitted values to dataset
  data$pnb <- pred$fit

  # Attach predistion variance to dataset
  data$v.pnb <- pred$se.fit^2

  # Attach overdispersion values to dataset
  data$overdispersion <- summary(fit)$dispersion
  }

  return(data)
}

addconditions <- function(data, spring=15:26, autumn=36:45, duration=5*52, last=NULL, delay=0){
  data<-addweeks(data)
  data$Cond3 <- ifelse(with(data, WoDi %in% c(spring, autumn)), 1, 0)
  if(is.null(last))
    data$Cond4 <- rep(1,nrow(data))

  data$Cond5<-with(data,ifelse(wk<max(wk)-delay,1,0))

  data$Cond6 <- with(data,ifelse(wk > max(wk)-duration - delay, 1, 0))

  return(data)

}

addweeks<-function(data){
  data$YWoDi<-with(data,sprintf("%04d-%02d",YoDi,WoDi))
  # Sort the data
  data <- data[order(data$YWoDi), ]

  data$wk <- c(1:nrow(data))
  return(data)
}
