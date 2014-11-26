output <- function(data) {
  # Temporal assignment of data, first run testing-base.R
  data <- final
  # Add additional information (to be added: should be part of data already)
  data <- within(data, {
    Version <- "v4-3"
    Model <- "LINE"
    source <- "HPA"
    country <- "England"
    group <- "Total"
    DateoA <- euromomoCntrl$dAggregation
    WoAi <- as.numeric(substr(ISOweek::date2ISOweek(DateoA), start = 7, stop = 8))
    YoAi <- as.numeric(substr(ISOweek::date2ISOweek(DateoA), start = 1, stop = 4))
  })

  # Graph: mortality
  plot.new()
  plot.window(xlim = c(1, nrow(data)), ylim = range(data$onb))
  # Get indices of labels for x-axis: isoweeks that are equal to the last isoweek
  which(data$WoDi == data$WoDi[nrow(data)]
  axis(side = 1, at = ), labels = ISOweek

  with(data, plot(x = 1:nrow(data), y = nb, type = "l", )
  with(data, {
    matplot(
      x = 1:nrow(data),
      y = cbind(nb, ifelse(CondDelays == 0, cnb, NA), pnb, u.pnb),
      type = "l",
      lty = 1,
      col = c("blue", "green", "orange", "yellow"))
    lines(x = 1:nrow(data), y = ifelse(CondSeason == 0, nb, NA), col = "black")
  })


