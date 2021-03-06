wavelet.plot.dh<-
function (wave.list, wavelet.levels = quantile(wave.list$Power, 
                                               probs = (0:10)/10), add.coi = TRUE, add.sig = TRUE, x.lab = gettext("Time"), 
          period.lab = gettext("Period"), crn.lab = gettext("RWI"), 
          key.cols = rev(rainbow(length(wavelet.levels) - 1)), key.lab = parse(text = paste0("\"", 
                                                                                             gettext("Power"), "\"^2")), add.spline = FALSE, f = 0.5, 
          nyrs = NULL, crn.col = "black", crn.lwd = 1, coi.col = "black", 
          crn.ylim = range(wave.list$y) * c(0.95, 1.05), side.by.side = FALSE, 
          useRaster = FALSE, res = 150, reverse.y = FALSE, ...) 
{
  y <- wave.list$y
  x <- wave.list$x
  period <- wave.list$period
  Signif <- wave.list$Signif
  coi <- wave.list$coi
  Power <- wave.list$Power
  siglvl <- wave.list$siglvl
  stopifnot(is.numeric(x), is.numeric(y), is.numeric(period), 
            is.numeric(Signif), is.numeric(coi), is.numeric(Power), 
            is.numeric(siglvl), is.logical(useRaster), length(useRaster) == 
              1, identical(side.by.side, TRUE) || identical(side.by.side, 
                                                            FALSE))
  stopifnot(is.numeric(wavelet.levels))
  n.x <- length(x)
  n.period <- length(period)
  dim.Power <- dim(Power)
  stopifnot(length(dim.Power) == 2, n.x == length(y), dim.Power[1] == 
              n.x, dim.Power[2] == n.period, length(Signif) == n.period, 
            length(coi) == n.x, length(siglvl) == 1, n.x >= 2, n.period >= 
              2)
  if (any(diff(x) <= 0) || any(diff(period) <= 0)) {
    stop("'wave.list$x' and 'wave.list$period' must be strictly ascending")
  }
  if (period[1] <= 0) {
    stop("'wave.list$period' must be positive")
  }
  coi[coi == 0] <- 1e-12
  Signif <- t(matrix(Signif, dim.Power[2], dim.Power[1]))
  Signif <- Power/Signif
  period2 <- log2(period)
  ytick <- unique(trunc(period2))
  ytickv <- 2^ytick
  coi2 <- log2(coi)
  coi2[coi2 < 0] <- 0
  coi2.yy <- c(coi2, rep(max(period2, na.rm = TRUE), n.x))
  coi2.yy[is.na(coi2.yy)] <- coi[2]
  yr.vec.xx <- c(x, rev(x))
  par.orig <- par(c("mar", "las", "mfrow", "mgp", "tcl"))
  on.exit(par(par.orig))
  nlevels <- length(wavelet.levels)
  seq.level <- seq_len(nlevels - 1)
  key.labs <- formatC(wavelet.levels, digits = 4, format = "f")
  asp <- NA
  xaxs <- "i"
  yaxs <- "i"
  las <- 1
  xlim <- range(x, finite = TRUE)
  ylim <- range(period2, finite = TRUE)
  if (isTRUE(reverse.y)) {
    ylim <- rev(ylim)
  }
  if (side.by.side) {
    layout(matrix(c(3, 2, 1), nrow = 1, byrow = TRUE), widths = c(1, 
                                                                  1, 0.2))
    scale.xlim <- c(0, 1)
    scale.ylim <- c(1, nlevels)
    scale.side <- 4
    scale.xleft <- 0
    scale.ybottom <- seq.level
    scale.xright <- 1
    scale.ytop <- 2:nlevels
    mar1 <- c(3, 1, 3, 3)
    mar2 <- c(3, 3, 3, 3)
    mar3 <- c(3, 3, 3, 3)
  }
  else {
    layout(matrix(c(3, 2, 1), ncol = 1, byrow = TRUE), heights = c(1, 
                                                                   1, 0.3))
    scale.xlim <- c(1, nlevels)
    scale.ylim <- c(0, 1)
    scale.side <- 1
    scale.xleft <- seq.level
    scale.ybottom <- 0
    scale.xright <- 2:nlevels
    scale.ytop <- 1
    mar1 <- c(3, 3, 0.1, 3)
    mar2 <- mar1
    mar3 <- c(0.1, 3, 4, 3)
  }
  par(mar = mar1, tcl = 0.5, mgp = c(1.5, 0.25, 0), las = las)
  dev.hold()
  on.exit(dev.flush(), add = TRUE)
  plot.new()
  plot.window(ylim = scale.ylim, xlim = scale.xlim, xaxs = xaxs, 
              yaxs = yaxs, asp = asp)
  rect(scale.xleft, scale.ybottom, scale.xright, scale.ytop, 
       col = key.cols)
  axis(scale.side, at = seq_along(wavelet.levels), labels = key.labs)
  if (side.by.side) {
    title(key.lab, cex.main = 1)
  }
  else {
    title(sub = key.lab, cex.sub = 1, line = 1.5)
  }
  par(mar = mar2, tcl = 0.5, mgp = c(1.5, 0.25, 0))
  plot.new()
  plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp, 
              las = las)
  if (is.na(useRaster)) {
    useRaster2 <- names(dev.cur()) %in% c("pdf", "postscript")
  }
  else {
    useRaster2 <- useRaster
  }
  cl <- quote(.filled.contour(as.double(x), as.double(period2), 
                              Power, as.double(wavelet.levels), key.cols))
  if (useRaster2) {
    args <- list(...)
    argNames <- names(args)
    if (is.null(argNames)) {
      args <- NULL
    }
    else {
      args <- args[!is.na(argNames) & nzchar(argNames)]
    }
    if (length(args) == 0L) {
      Call <- as.call(c(as.name("rasterPlot"), alist(expr = cl, 
                                                     res = res, antialias = "none", interpolate = FALSE)))
    }
    else {
      Call <- as.call(c(as.name("rasterPlot"), args))
      Call <- as.list(match.call(rasterPlot, Call))
      anam <- names(Call[-1L])
      Call[["expr"]] <- quote(cl)
      Call[["res"]] <- quote(res)
      Call[["region"]] <- "plot"
      Call[["draw"]] <- TRUE
      if (!("antialias" %in% anam)) {
        Call[["antialias"]] <- "none"
      }
      if (!("interpolate" %in% anam)) {
        Call[["interpolate"]] <- FALSE
      }
      Call <- as.call(Call)
    }
    tryCatch(eval(Call), error = function(e) {
      message(as.character(e), appendLF = FALSE)
      message("reverting to useRaster=FALSE")
      eval(cl)
    })
  }
  else {
    eval(cl)
  }
  if (isTRUE(add.sig)) {
    contour(x, period2, Signif, levels = 1, labels = siglvl, 
            drawlabels = FALSE, axes = FALSE, frame.plot = FALSE, 
            add = TRUE, lwd = 2, col = "black")
  }
  if (isTRUE(add.coi)) {
    polygon(yr.vec.xx, coi2.yy, density = c(10, 20), angle = c(-45, 
                                                               45), col = coi.col)
  }
  axis(1)
  axis(2, at = ytick, labels = ytickv)
  if (side.by.side) {
    axis(3, at = seq(from = 0, to = length(mor_data$x), by = length(mor_data$x)/5), labels = seq(from = as.Date(st_fun), by = "month", length.out = 6),cex.axis=1)
    axis(4, at = ytick, labels = ytickv)
  }
  else {
    axis(3, labels = NA) #
    axis(4, at = ytick, labels = NA)
  }
  title(xlab = x.lab, ylab = period.lab)
  box()
  par(mar = mar3, las = 0)
  plot(x, y, type = "l", xlim, xaxs = xaxs, yaxs = yaxs, asp = asp, 
       xlab = "", ylab = "", axes = FALSE, col = crn.col, lwd = crn.lwd, 
       ylim = crn.ylim, main = main_fun)
  if (add.spline) {
    spl <- y
    tmp <- na.omit(spl)
    if (is.null(nyrs)) {
      nyrs2 <- length(tmp) * 0.33
    }
    else {
      nyrs2 <- nyrs
    }
    tmp <- ffcsaps(y = tmp, x = seq_along(tmp), nyrs = nyrs2, 
                   f = f)
    spl[!is.na(spl)] <- tmp
    lines(x, spl, col = "red", lwd = 2)
  }
  axis(3, at = seq(from = 0, to = length(mor_data$x), by = length(mor_data$x)/5), labels = seq(from = as.Date(st_fun), by = "month", length.out = 6),cex.axis=1)
  axis(4)
  if (side.by.side) {
    axis(1)
    axis(2)
    title(xlab = x.lab, ylab = crn.lab)
  }
  else {
    axis(1, labels = NA)
    axis(2, labels = NA)
    mtext(crn.lab, side = 4, line = 1.5, cex = 0.75)
  }
  box()
  invisible()
}
