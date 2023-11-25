fit2 <- function (datas = data, state = s, parms = p, odes = model, 
          free = NULL, who = NULL, differ = NULL, fixed = NULL, tmin = 0, 
          tmax = NULL, ymin = NULL, ymax = NULL, log = "", xlab = "Time", 
          ylab = "Density", bootstrap = 0, show = NULL, fun = NULL, 
          costfun = cost, initial = FALSE, add = FALSE, timeplot = TRUE, 
          legend = TRUE, main = NULL, sub = NULL, pchMap = NULL,c.legend.pos = NULL, ...) 
{
  dots <- list(...)
  if (!is.null(dots)) {
    unknown <- names(dots[!names(dots) %in% c(args_fit, 
                                              args_run, args_plot)])
    if (length(unknown) > 0) 
      warning(paste("Unknown argument(s):", unknown, sep = " "))
    dots_run <- dots[names(dots) %in% args_run]
    dots_fit <- dots[names(dots) %in% c(args_fit, args_run)]
    if ("method" %in% names(dots)) {
      if (!(dots$method %in% c(methods_run, methods_fit))) 
        stop(paste("Unknown method:", dots$method))
      if (!(dots$method %in% methods_fit)) {
        dots_fit[["run_method"]] <- dots$method
        dots_fit[["method"]] <- NULL
      }
      if (!(dots$method %in% methods_run)) 
        dots_run[["method"]] <- NULL
    }
  }
  if (is.null(free) & !is.null(who)) 
    free <- who
  if (!is.null(fun)) 
    fun <- match.fun(fun)
  if (is.data.frame(datas)) 
    datas <- list(datas)
  nsets <- length(datas)
  all <- c(state, parms)
  allNms <- names(all)
  if (initial) 
    totp <- parms
  else totp <- all
  isVar <- setNames(c(rep(TRUE, length(state)), rep(FALSE, 
                                                    length(parms))), allNms)
  if (is.null(free) & is.null(differ)) 
    free <- allNms
  if (initial) 
    free <- idrop(names(state), free)
  ifree <- index(free, names(totp))
  if (!is.null(fixed)) 
    if (!is.list(fixed)) 
      stop("fixed should be a list")
  else {
    if (length(intersect(names(fixed), free)) > 0) 
      stop("fixed should not overlap with free")
    if (length(intersect(names(fixed), differ)) > 0) 
      stop("fixed should not overlap with differ")
  }
  if (is.null(differ)) 
    guess <- setNames(rep(0, length(free)), free)
  else {
    if (!is.list(differ)) 
      ldiff <- makelist(differ, state = state, parms = parms, 
                        nsets = nsets)
    else {
      ldiff <- differ
      differ <- names(ldiff)
    }
    free <- idrop(differ, free)
    guess <- setNames(rep(0, length(free) + nsets * length(differ)), 
                      c(free, rep(differ, nsets)))
  }
  lenfree <- length(free)
  lendiff <- length(differ)
  VarsFree <- free[isVar[free]]
  ParsFree <- free[!isVar[free]]
  if (length(VarsFree) > 0) 
    guess[VarsFree] <- state[VarsFree]
  if (length(ParsFree) > 0) 
    guess[ParsFree] <- parms[ParsFree]
  if (!is.null(differ)) 
    for (inum in seq(lendiff)) for (iset in seq(nsets)) guess[lenfree + 
                                                                inum + (iset - 1) * lendiff] <- ldiff[[differ[inum]]][iset]
  logy <- ifelse(grepl("y", log), TRUE, FALSE)
  f <- do.call("modFit", c(list(f = costfun, p = guess, datas = datas, 
                                odes = odes, state = state, parms = parms, free = free, 
                                differ = differ, fixed = fixed, fun = fun, initial = initial, 
                                isVar = isVar), dots_fit))
  found <- f$par
  cat("SSR:", f$ssr, " Estimates:\n")
  print(found)
  if (timeplot) {
    tmaxn <- ifelse(is.null(tmax), max(unlist(lapply(seq(nsets), 
                                                     function(i) {
                                                       max(datas[[i]][1])
                                                     }))), tmax)
    ymaxn <- ifelse(is.null(ymax), max(unlist(lapply(seq(nsets), 
                                                     function(i) {
                                                       max(datas[[i]][2:ncol(datas[[i]])])
                                                     }))), ymax)
    yminn <- ifelse(is.null(ymin), min(unlist(lapply(seq(nsets), 
                                                     function(i) {
                                                       min(datas[[i]][2:ncol(datas[[i]])])
                                                     }))), ymin)
    for (iset in seq(nsets)) {
      data <- datas[[iset]]
      if (length(VarsFree) > 0) 
        state[VarsFree] <- found[VarsFree]
      if (length(ParsFree) > 0) 
        parms[ParsFree] <- found[ParsFree]
      if (!is.null(fixed)) 
        for (inum in seq(length(fixed))) {
          name <- names(fixed)[inum]
          if (isVar[name]) 
            state[match(name, names(state))] <- fixed[[inum]][iset]
          else parms[match(name, names(parms))] <- fixed[[inum]][iset]
        }
      if (!is.null(differ)) 
        for (i in seq(lendiff)) {
          value <- found[lenfree + i + (iset - 1) * 
                           lendiff]
          if (isVar[differ[i]]) 
            state[match(differ[i], names(state))] <- value
          else parms[match(differ[i], names(parms))] <- value
        }
      if (initial) {
        if (data[1, 1] > 0) 
          stop("Data doesn't start at time=0")
        state[1:length(state)] <- unlist(data[1, 2:ncol(data)])
      }
      tmaxi <- ifelse(is.null(tmax), ifelse(add, tmaxn, 
                                            max(data[, 1])), tmax)
      nsol <- do.call("run", c(list(tmax = tmaxi, state = state, 
                                    parms = parms, odes = odes, table = TRUE, timeplot = FALSE), 
                               dots_run))
      ymaxi <- ifelse(is.null(ymax), ifelse(add, ymaxn, 
                                            max(data[2:ncol(data)], nsol[2:ncol(nsol)])), 
                      ymax)
      ymini <- ifelse(is.null(ymin), ifelse(add, yminn, 
                                            min(data[2:ncol(data)], nsol[2:ncol(nsol)])), 
                      ymin)
      solnames <- names(nsol)[2:ncol(nsol)]
      colnames <- names(data)[2:ncol(data)]
      imain <- main[min(length(main), iset)]
      isub <- sub[min(length(sub), iset)]
      if (is.null(show)) {
        timePlot2(nsol, tmin = tmin, tmax = tmaxi, ymin = ymini, 
                 ymax = ymaxi, log = log, main = imain, sub = isub, 
                 add = ifelse(iset > 1, add, FALSE), xlab = xlab, 
                 ylab = ylab, font.main = font.main, font.sub = font.sub, 
                 legend = legend, c.legend.pos = c.legend.pos)
        timePlot2(data, draw = points, add = TRUE, legend = FALSE, 
                 lwd = 1.5, colMap = index(colnames, solnames), 
                 pchMap = pchMap, c.legend.pos = c.legend.pos)
      }
      else {
        for (i in show) {
          timePlot2(nsol, tmin = tmin, tmax = tmaxi, 
                   ymin = ymini, ymax = ymaxi, log = log, show = i, 
                   main = imain, sub = isub, xlab = xlab, ylab = ylab, 
                   font.main = font.main, font.sub = font.sub, 
                   legend = legend, c.legend.pos = c.legend.pos)
          if (i %in% colnames) 
            timePlot2(data, draw = points, add = TRUE, 
                     legend = FALSE, lwd = 1.5, show = i, colMap = index(colnames, 
                                                                         solnames), pchMap = pchMap, c.legend.pos = c.legend.pos)
        }
      }
    }
  }
  if (bootstrap == 0) 
    return(f)
  imat <- sapply(seq(bootstrap), function(i) {
    samples <- lapply(seq(nsets), function(j) {
      datas[[j]][sample(nrow(datas[[j]]), replace = TRUE), 
      ]
    })
    ifit <- do.call("modFit", c(list(f = costfun, p = found, 
                                     datas = samples, odes = odes, state = state, parms = parms, 
                                     free = free, differ = differ, fixed = fixed, fun = fun, 
                                     initial = initial, isVar = isVar), dots_fit))
    ifit$par
  })
  print(apply(imat, 1, function(i) c(mean = mean(i), sd = sd(i), 
                                     median = median(i), quantile(i, c(0.025, 0.975)))))
  f$bootstrap <- t(imat)
  return(f)
}

environment(fit2) <- asNamespace('Grind')
