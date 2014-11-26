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

  # Graph: crude mortality
  layout(mat = matrix(c(1, 2), nrow = 2, ncol = 1), heights = c(4, 1))
  # Graph
  par(mar = c(1.5, 2.5, 3, 2.5))
  plot.new()
  with(data, {
    plot.window(xlim = c(1, nrow(data)), ylim = range(cnb))
    idx <- which(WoDi == WoDi[nrow(data)])
    axis(side = 1, at = idx, labels = ISOweek[idx])
    axis(side = 2)
    box()
    title(main = paste("Number of deaths -", ISOweek[nrow(data)], "\n", country[1], "- Group", group[1]))
    # Add the graphs
    lines(x = 1:nrow(data), y = onb, col = "black")
    lines(x = 1:nrow(data), y = ifelse(CondDelays == 0, cnb, NA), col = "green")
    lines(x = 1:nrow(data), y = ifelse(CondSeason == 1, onb, NA), col = "blue")
    lines(x = 1:nrow(data), y = pnb, col = "red")
    # Only plot prediction intervals that are smaller than max(onb)
    for (multiplier in seq(from = 2, to = 20, by = 2)) {
      z <- (pnb^(2/3)+ multiplier*pv.pnb)^(3/2)
      if (all(z < max(onb))) lines(x = 1:nrow(data), y = z, col = "yellow")
    }
  })
  # Legend
  par(mar = rep(0, 4))
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  legend(
    x = "center",
    xjust = 0.5, yjust = 0.5,
    legend = c("Known number of deaths", "Data used in model", "Corrected number deaths",
      "Baseline", "Prediction interval by 2 stdv"),
    lty = 1,
    col = c("black", "blue", "green", "red", "yellow"),
    ncol = 2
  )

  # Graph: z-scores
  layout(mat = matrix(c(1, 2), nrow = 2, ncol = 1), heights = c(4, 1))
  # Graph
  par(mar = c(1.5, 2.5, 3, 2.5))
  plot.new()
  with(data, {
    plot.window(xlim = c(1, nrow(data)), ylim = range(Zscore))
    idx <- which(WoDi == WoDi[nrow(data)])
    axis(side = 1, at = idx, labels = ISOweek[idx])
    axis(side = 2, at = seq(from = -20, to = 20, by = 2))
    abline(h =  seq(from = -20, to = 20, by = 2), col = "yellow")
    abline(h = 0, col = "red")
    box()
    title(main = paste("Z-score -", ISOweek[nrow(data)], "\n", country[1], "- Group", group[1]))
        # Add the graphs
    lines(x = 1:nrow(data), y = Zscore, col = "black")
    lines(x = 1:nrow(data), y = ifelse(CondSeason == 1, Zscore, NA), col = "blue")
  })
  # Legend
  par(mar = rep(0, 4))
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  legend(
    x = "center",
    xjust = 0.5, yjust = 0.5,
    legend = c("Z-score", "Data used in model", "Baseline", "Standard deviations by 2"),
    lty = 1,
    col = c("black", "blue", "red", "yellow"),
    ncol = 2
  )




