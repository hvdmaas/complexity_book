# -------------------------------------------------------------------------
# Grind::run2 & Grind::timePlot2 ------------------------------------------
# -------------------------------------------------------------------------

# 'timePlot' function required by 'run' ------------------------------------
timePlot2 <- function (data, tmin = 0, tmax = NULL, ymin = 0, ymax = NULL, 
                       log = "", xlab = "Time", ylab = "Density", show = NULL, 
                       legend = TRUE, draw = lines, lwd = 1, add = FALSE, 
                       main = NULL, sub = NULL, recolor = FALSE, colMap = NULL, 
                       pchMap = NULL, bty = 'n', c.legend.pos = NULL, ...) {
  
  font_add(family = "CMU-bright", regular = "cmunbmr.ttf")
  showtext_auto()
  #Check if font was added
  font_families()
  colors <- c("#332288", "#882255", "#FFCC01", "#666666", #1-4
               "#44AA99", "#CC6677",                       #5,6
               "#117733", "#88CCEE", "#AA4499" )          #7,8,9
  if (!is.null(draw)) 
    draw <- match.fun(draw)
  
  if (is.null(tmax)) 
    tmax <- max(data[, 1])
  if (is.null(ymax)) 
    ymax <- max(data[, 2:ncol(data)])
  if (tmin == 0 & grepl("x", log)) 
    tmin <- min(data[, 1])
  if (ymin == 0 & grepl("y", log)) 
    ymin <- min(data[, 2:ncol(data)])
  
  if (!add) {
    plot(1, 1, type = "n", xlim = c(tmin, tmax), ylim = c(ymin, ymax), 
         log = log, xlab = xlab, ylab = ylab, main = main, sub = sub, 
         font.main = font.main, font.sub = font.sub, 
         bty = 'n', family = "CMU-bright", axes = FALSE, cex.lab = 1.2 ,# Tasos' addition
         ...)
    axis(1, at = NULL, labels = TRUE, tcl = 0, cex.axis = 1)  # custom axis
    axis(2, at = NULL, labels = TRUE, tcl = 0, cex.axis = 1)  #    -//-
  }
  colnames <- names(data)[2:ncol(data)]
  if (!is.null(show)) 
    ishows <- index(show, colnames)
  else ishows <- seq(ncol(data) - 1)
  if (recolor) {
    for (i in seq(length(ishows))) draw(data[, 1], data[, 
                                                        ishows[i] + 1], col = colors[i],
                                        lty =2*i-1, #tasos: for solid & dotted (1,3,5,...)
                                        lwd = lwd, pch = i)
    if (legend) {
      if(is.null(c.legend.pos)) c.legend.pos = 'topright'
      if (identical(draw, lines)) 
        legend(c.legend.pos, legend = colnames[ishows], 
               col = colors, lty = seq(1,100,2), # odd numbering again (to avoid dashed line = 2)
               lwd = lwd, cex = sizeLegend, bty = 'n')
      else legend(c.legend.pos, legend = colnames[ishows], 
                  col = colors, lty = seq(1,100,2), # odd numbering again (to avoid dashed line = 2)
                  lwd = lwd, cex = sizeLegend, 
                  pch = 1:100, bty = 'n')
    }
    return()
  }
  for (i in ishows) {
    j <- ifelse(is.null(colMap), i, colMap[i])
    k <- ifelse(is.null(pchMap), j, pchMap[i])
    draw(data[, 1], data[, i + 1], 
         col = colors[min(j, ncolors)], # need check
         lwd = lwd, pch = k, 
         lty =2*i-1 #tasos: same trick
         )
  }
  if (legend) {
    if(is.null(c.legend.pos)) c.legend.pos = 'topright'
    if (identical(draw, lines)) 
      legend(c.legend.pos, legend = colnames[ishows], col = colors[ishows], 
             lty = seq(1,100,2), # odd numbering again (to avoid dashed line = 2)
             lwd = lwd, cex = sizeLegend, bty = 'n')
    else legend(c.legend.pos, legend = colnames[ishows], col = colors[ishows], 
                lty = seq(1,100,2), # odd numbering again (to avoid dashed line = 2)
                lwd = lwd, cex = sizeLegend, pch = ishows, bty = 'n')
  }
}
environment(timePlot2) <- asNamespace('Grind')
#timePlot2(dat1)
# 'run2' function ---------------------------------------------------------
run2 <- function (tmax = 100, tstep = 1, state = s, parms = p, odes = model, 
          ymin = 0, ymax = NULL, log = "", x = 1, y = 2, xlab = "Time", 
          ylab = "Density", tmin = 0, draw = lines, times = NULL, show = NULL, 
          arrest = NULL, after = NULL, tweak = NULL, timeplot = TRUE, 
          traject = FALSE, table = FALSE, add = FALSE, legend = TRUE, 
          solution = FALSE, delay = FALSE, lwd = 1, col = "grey5", 
          pch = 20, c.legend.pos = NULL, ...) 
{
  if (delay & (solution | !is.null(after))) 
    stop("Don't use solution or after with delay equations")
  dots <- list(...)
  if (!is.null(dots)) {
    unknown <- names(dots[!names(dots) %in% c(args_run, args_plot)])
    if (length(unknown) > 0) 
      warning(paste("Unknown argument(s):", unknown, sep = " "))
    dots_run <- dots[names(dots) %in% args_run]
  }
  else dots_run <- NULL
  nvar <- length(state)
  if (!is.numeric(x)) 
    x <- index(x, names(state))
  if (!is.numeric(y)) 
    y <- index(y, names(state))
  if (is.null(times)) 
    times <- seq(tmin, tmax, by = tstep)
  else {
    times <- sort(times)
    tmin <- min(times)
    tmax <- max(times)
  }
  if (!is.null(arrest)) {
    if (is.numeric(arrest)) 
      times <- sort(c(arrest, times))
    else times <- sort(c(as.numeric(parms[arrest]), times))
    times <- unique(round(times, 8))
  }
  if (solution) {
    if (!is.null(after)) 
      stop("Don't combine the option after with solution")
    nsol <- sapply(times, odes, state, parms)
    if (length(state) > 1) 
      nsol <- data.frame(times, t(nsol))
    else nsol <- data.frame(times, nsol)
    names(nsol) <- c("time", names(state))
  }
  else {
    if (is.null(after)) {
      if (!delay) 
        nsol <- as.data.frame(do.call("ode", c(list(times = times, 
                                                    func = odes, y = state, parms = parms), dots_run)))
      else nsol <- as.data.frame(do.call("dede", c(list(times = times, 
                                                        func = odes, y = state, parms = parms), dots_run)))
    }
    else {
      t0 <- times[1]
      for (t in times[2:length(times)]) {
        nsol0 <- do.call("ode", c(list(times = c(t0, 
                                                 t), func = odes, y = state, parms = parms), 
                                  dots_run))
        if (t0 == times[1]) 
          nsol <- nsol0
        state <- nsol0[2, 2:(nvar + 1)]
        if (!is.null(after)) 
          eval(parse(text = after))
        if (t0 == times[1]) 
          nsol[2, 2:(nvar + 1)] <- state
        else nsol <- rbind(nsol, c(t, state))
        t0 <- t
      }
      nsol <- as.data.frame(nsol)
    }
  }
  if (!is.null(tweak)) 
    eval(parse(text = tweak))
  if (timeplot & !traject) 
    do.call("timePlot2", c(list(data = nsol, tmin = tmin, 
                               tmax = tmax, ymin = ymin, ymax = ymax, log = log, 
                               add = add, xlab = xlab, ylab = ylab, show = show, 
                               draw = draw, lwd = lwd, legend = legend, font.main = font.main, 
                               font.sub = font.sub, c.legend.pos = c.legend.pos), dots[names(dots) %in% args_plot]))
  if (traject) {
    points(nsol[1, x + 1], nsol[1, y + 1], pch = pch)
    lines(nsol[, x + 1], nsol[, y + 1], lwd = lwd, col = col)
  }
  if (table) 
    return(nsol)
  f <- state
  f[1:length(f)] <- as.numeric(nsol[nrow(nsol), 2:(nvar + 1)])
  return(f)
}
environment(run2) <- asNamespace('Grind')
#run2()
