plane2 <- function (xmin = 0, xmax = 1.1, ymin = 0, ymax = 1.1, log = "", 
                    npixels = 500, state = s, parms = p, odes = model, x = 1, 
                    y = 2, time = 0, grid = 5, eps = 0, show = NULL, portrait = FALSE, 
                    vector = FALSE, add = FALSE, legend = TRUE, zero = TRUE,
                    lwd = 1, col = "gray20", pch = 20, bty='n', ...) 
{
  
  font_add(family = "CMU-bright", regular = "cmunbmr.ttf")
  showtext_auto()
  #Check if font was added
  font_families()
  
  # Colors ------------------------------------------------------------------
  # Important objects
  #colors <- RColorBrewer::brewer.pal(8, 'Dark2')[c(3,4,7,8)]
  ncolors <- c("#332288", "#882255", "#FFCC01", "#666666", #1-4
               "#44AA99", "#CC6677",                       #5,6
               "#117733", "#88CCEE", "#AA4499" )          #7,8,9
  #cfont <- "CMU-bright"
  
  dots <- list(...)
  if (!is.null(dots)) {
    unknown <- names(dots[!names(dots) %in% c(args_run, args_plot)])
    if (length(unknown) > 0) 
      warning(paste("Unknown argument(s):", unknown, sep = " "))
  }
  if (!is.null(dots)) 
    dots_run <- dots[names(dots) %in% args_run]
  else dots_run <- NULL
  if (!is.numeric(x)) 
    x <- index(x, names(state))
  if (!is.numeric(y)) 
    y <- index(y, names(state))
  if (!is.null(show)) 
    ishows <- index(show, names(state))
  else ishows <- c(x, y)
  nvar <- length(state)
  if (zero) 
    state[1:nvar] <- rep(0, nvar)
  lvec <- 50
  logx <- ifelse(grepl("x", log), TRUE, FALSE)
  logy <- ifelse(grepl("y", log), TRUE, FALSE)
  if (logx) 
    xc <- 10^seq(log10(xmin), log10(xmax), length.out = npixels)
  else xc <- seq(xmin + eps, xmax, length.out = npixels)
  if (logy) 
    yc <- 10^seq(log10(ymin), log10(ymax), length.out = npixels)
  else yc <- seq(ymin + eps, ymax, length.out = npixels)
  xvar <- names(state)[x]
  yvar <- names(state)[y]
  if (!add) {
    do.call("plot", c(list(1, 1, type = "n", xlim = c(xmin, 
                                                      xmax), ylim = c(ymin, ymax), xlab = xvar, ylab = yvar, 
                           log = log, font.main = font.main, font.sub = font.sub, bty = 'n',  cex.lab = 1.2, family = "CMU-bright", axes = FALSE), 
                      dots[names(dots) %in% args_plot]))
    
    axis(1, at = NULL, labels = TRUE, tcl = 0, cex.axis = 1) # custom x
    axis(2, at = NULL, labels = TRUE, tcl = 0, cex.axis = 1) # custom y
    if (legend) 
      legend("topright", legend = names(state)[ishows], 
             col = ncolors[ishows],lwd = lwd, cex = sizeLegend, 
             lty = seq(1,100,2)) # odd numbering again (to avoid dashed line = 2))

  }
  vstate <- as.list(state)
  npixels2 <- npixels^2
  for (j in seq(1, nvar)) if (j != x & j != y) 
    vstate[[j]] <- rep(vstate[[j]], npixels2)
  FUN <- function(xc, yc, i) {
    vstate[[x]] <- xc
    vstate[[y]] <- yc
    odes(time, vstate, parms)[[1]][seq((i - 1) * npixels2 + 
                                         1, i * npixels2)]
  }
  for (i in ishows) contour(xc, yc, outer(xc, yc, FUN, i), 
                            levels = 0, drawlabels = FALSE, add = TRUE, col = ncolors[i], 
                            lwd = lwd, lty = 2*i-1)
  if (portrait | vector) {
    if (logx) {
      dx <- (log10(xmax) - log10(xmin))/grid
      vx <- 1 + 3.32 * grid * dx/lvec
    }
    else {
      dx <- (xmax - xmin)/grid
      vx = grid * dx/lvec
    }
    if (logy) {
      dy <- (log10(ymax) - log10(ymin))/grid
      vy <- 1 + 3.32 * grid * dy/lvec
    }
    else {
      dy <- (ymax - ymin)/grid
      vy = grid * dy/lvec
    }
    for (i in seq(1, grid)) {
      if (logx) 
        state[x] <- 10^((i - 1) * dx + dx/2 + log10(xmin))
      else state[x] <- (i - 1) * dx + dx/2 + xmin
      for (j in seq(1, grid, 1)) {
        if (logy) 
          state[y] <- 10^((j - 1) * dy + dy/2 + log10(ymin))
        else state[y] <- (j - 1) * dy + dy/2 + ymin
        if (portrait) {
          points(state[x], state[y], pch = pch, col = col)
          nsol <- do.call("run", c(list(state = state, 
                                        parms = parms, odes = odes, timeplot = FALSE, 
                                        table = TRUE), dots_run))
          lines(cbind(nsol[x + 1], nsol[y + 1]), col = col,lwd=.5)
        }
        if (vector) {
          dt <- sign(unlist(odes(time, state, parms)))
          if (logx) 
            lines(c(state[x], state[x] * vx^dt[x]), c(state[y], 
                                                      state[y]), lwd=.5, col = col)
          else lines(c(state[x], state[x] + vx * dt[x]), 
                     c(state[y], state[y]),lwd=.5, col = col)
          if (logy) 
            lines(c(state[x], state[x]), c(state[y], 
                                           state[y] * vy^dt[y]),lwd=.5, col = col)
          else lines(c(state[x], state[x]), c(state[y], 
                                              state[y] + vy * dt[y]),lwd=.5, col = col)
        }
      }
    }
  }
}

environment(plane2) <- asNamespace('Grind')
