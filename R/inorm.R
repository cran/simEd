###############################################################################
# R function to visualize the normal idf evaluated at a provided uniform(0,1)
# u. This will graph the idf in action (via dashed lines back across the cdf).
# Note that the u argument can be a scalar or vector.  If a vector, multiple
# dashed lines will be displayed, and the return type will be a vector.
# The function also gives the option of displaying a histogram of the variates
# generated, with theoretical normal superimposed.
###############################################################################
inorm <- function(u = runif(1), mean = 0, sd = 1,
                  minPlotQuantile = 0.01, 
                  maxPlotQuantile = 0.99,
                  plot            = TRUE,
                  showCDF         = TRUE,
                  showPDF         = FALSE,
                  showECDF        = FALSE,
                  show            = NULL,
                  plotDelay       = 0,
                  maxPlotTime     = 30,
                  showTitle       = TRUE,
                  respectLayout   = TRUE)
{
  #############################################################################

  # first, some parameter checking...

  if (!is.null(u) && (min(u) <= 0 || max(u) >= 1)) stop("must have 0 < u < 1")

  if (!is.numeric(mean) || !is.numeric(sd) || 
      length(mean) != 1 || length(sd) != 1 || sd < 0)
    stop("'mean' and 'sd' must each be numeric values with 'sd' >= 0")

  if (!is.numeric(minPlotQuantile) || length(minPlotQuantile) != 1 ||
         minPlotQuantile <= 0 || minPlotQuantile >= 1)
    stop("'minPlotQuantile' must be a numeric value in (0,1)")
  if (!is.numeric(maxPlotQuantile) || length(maxPlotQuantile) != 1 ||
         maxPlotQuantile <= 0 || maxPlotQuantile >= 1)
    stop("'maxPlotQuantile' must be a numeric value in (0,1)")
  if (minPlotQuantile >= maxPlotQuantile) 
    stop("'minPlotQuantile' must be less than 'maxPlotQuantile'")

  if (!is.logical(plot) || length(plot) != 1)
    stop("'plot' must be single a logical value")
  if (!is.logical(showCDF) || length(showCDF) != 1)
    stop("'showCDF' must be a single logical value")
  if (!is.logical(showPDF) || length(showPDF) != 1)
    stop("'showPDF' must be a single logical value")
  if (!is.logical(showECDF) || length(showECDF) != 1)
    stop("'showECDF' must be a single logical value")

  if (!is.null(show)) {
    if (!is.numeric(show) || (length(show) != 3 && length(show) != 1) ||
        sum(show) < 0 || sum(show) > 7)
      stop(paste("'show' must be a binary vector of length three or",
                 "a single integer in [0,7] a la Unix's chmod"))
    if (length(show) == 3 && (min(show) < 0 || max(show) > 1))
      stop("when 'show' is a binary vector, components must be 0 or 1")
    if (length(show) == 1 && show != floor(show))
      stop("when 'show' is not a binary vector, it must be an integer in [0,7]")
  }

  if (!is.logical(showTitle) || length(showTitle) != 1)
    stop("'showTitle' must be a single logical value")
  if (!is.logical(respectLayout) || length(respectLayout) != 1)
    stop("'respectLayout' must be a single logical value")

  if (!is.numeric(plotDelay) || length(plotDelay) != 1 || plotDelay < 0)
    stop("'plotDelay' must be a numeric value (in secs) >= 0")
  if (!is.numeric(maxPlotTime) || length(maxPlotTime) != 1 || maxPlotTime < 0)
    stop("'maxPlotTime' must be a numeric value (in secs) >= 0")

  #############################################################################

  warnVal <- options("warn")  # save current warning setting...
  options(warn = 1)          # set to immediate warnings

  # compute the plot min and max
  fromX <- qnorm(minPlotQuantile, mean = mean, sd = sd)
  toX   <- qnorm(maxPlotQuantile, mean = mean, sd = sd)

  # check 'show' entry if not null
  if (length(show) == 3) {
    indivSum = 4*as.integer(showCDF) + 2*as.integer(showPDF) + as.integer(showECDF)
    showSum  = 4*show[1] + 2*show[2] + show[3]
    if ((!missing(showCDF) || !missing(showPDF) || !missing(showECDF))
        && indivSum != showSum)
      warning(paste("use of 'show' does not match 'showCDF','showPDF','showECDF';", 
                    "'show' values will take precedence"))
    showCDF  <- as.logical(show[1])  # a 0 or 1 entry
    showPDF  <- as.logical(show[2])
    showECDF <- as.logical(show[3])
  } else if (length(show) == 1) {
    indivSum = 4*as.integer(showCDF) + 2*as.integer(showPDF) + as.integer(showECDF)
    if ((!missing(showCDF) || !missing(showPDF) || !missing(showECDF))
        && indivSum != show)
      warning(paste("use of 'show' does not match 'showCDF','showPDF','showECDF';", 
                    "'show' values will take precedence"))
    # treat a la chmod command from Unix (where they probably do bit shifting)
    showCDF <- showPDF <- showECDF <- FALSE
    if (show >= 4) { showCDF  <- TRUE; show <- show - 4}
    if (show >= 2) { showPDF  <- TRUE; show <- show - 2}
    if (show >= 1) { showECDF <- TRUE }
  }

  if (is.null(u)) {
    # this corresponds to just showing the pdf and/or cdf distribution
    if (showECDF == TRUE)
      warning("ignoring showECDF = TRUE since u is NULL; plotting CDF")
    if (plot == FALSE) {
      warning("ignoring plot = FALSE since u is NULL, indicating distribution plot(s)")
      plot <- TRUE
    }
    if (showCDF == TRUE || showECDF == TRUE) {
      # may ask to show CDF via either showCDF or showECDF, but we
      # plot the distro-only (when no variates) CDF via showECDF logic below
      showCDF  <- FALSE
      showECDF <- TRUE
    }
    # by default, if u is null and showPDF is missing (using default FALSE),
    # let's set showPDF to TRUE -- the user can override by giving F explicitly
    if (missing(showPDF)) showPDF <- TRUE

    plotDelay <- 0
    xVals     <- NULL
  } else {
    # generate the variates
    xVals <- qnorm(u, mean = mean, sd = sd)
  }

  #############################################################################

  # if no plot, or "equal" min/max quantile values, just return the variates...

  if (plot == FALSE) 
    if (is.null(xVals)) return(invisible(xVals)) else return(xVals)

  if (showCDF == FALSE && showPDF == FALSE && showECDF == FALSE)
  {
    if (plot == TRUE) 
      warning("ignoring plot since showCDF, showPDF, and showECDF are all FALSE")
    options(warn = warnVal$warn)  # reset warnings to user's choice
    if (is.null(xVals)) return(invisible(xVals)) else return(xVals)
  }

  if (round(fromX, digits = 7) == round(toX, digits = 7))
  {
    warning(paste("politely declining to plot:",
                "essentially equal min/max quantile values"))
    options(warn = warnVal$warn)  # reset warnings to user's choice
    if (is.null(xVals)) return(invisible(xVals)) else return(xVals)
  }

  #############################################################################

  # otherwise, user wishes to plot...
  
  if (plotDelay > 0 && showCDF == FALSE)
    warning("ignoring non-zero plotDelay since showCDF is FALSE")
  else if (plotDelay * length(u) > maxPlotTime) {
    warning(paste("politely ignoring u/plotDelay combination resulting",
        "in > maxPlotTime =", maxPlotTime, "secs plotting time"))
    plotDelay <- 0
  }

  # use sum to determine how many plot "shows" are TRUE ==> set # rows
  minPlots  <- sum(showCDF, showPDF, showECDF)

  if (respectLayout) {
    # try to respect user's previously defined par(mfrow) or par(mfcol) if 
    # sufficient to show all plots; if not, display a warning message
    userPlots <- prod(par("mfrow"))
    if (userPlots < minPlots) {
      msg <- paste('Cannot display the requested ', minPlots, 
              ' plots simultaneously because layout is set for ', userPlots, 
              ' plot', if (userPlots > 1) 's. ' else '. ',
              'Please use \'par\' to set layout appropriately, e.g., ',
              'par(mfrow = c(', minPlots, ',1)) or ', 
              'par(mfcol = c(1,', minPlots, ')).', sep = "")
      warning(msg)
    }
  } else {
    # force the layout sufficient to show only these plots
    par(mfrow = c(minPlots, 1))  # minPlots rows, 1 column
  }

  # set default margins for plots
  botMar <- if (minPlots > 1) 4.1 else 5.1
  par(mar = c(botMar, 4.1, 2.1, 1.5))

  # set color for plotting variates / estimated curves
  color <- "red3"
  colorTrans <- rgb(col2rgb(color)[1]/255,
                    col2rgb(color)[2]/255,
                    col2rgb(color)[3]/255,
                    if (length(u) < 150) 0.5 else 0.35)
  colorOutline <- if (length(u) < 150) color else colorTrans

  # set line width and cex paramters for plotting
  lwd <- max(2, round(log10(length(xVals))))  # increases with num xVals
  plot_cex <- par("cex")  # R automatically lowers if three plots...

  # create the main title
  dispSd <- round(sd, digits = 3)
  if (dispSd == 0) { dispSd <- format(sd, digits = 3, scientific = TRUE) }
  titleStr <- paste("normal(mean = ", as.character(mean), 
              ", sd = ", as.character(dispSd), ")\n", sep = "")
  if (!is.null(xVals)) 
    titleStr <- paste(titleStr, "random variate generation", sep = "")

  # grab onto the histogram data for use in cdf and/or pdf plots;
  # start this here so we can use hist's rightmost bin point in xlim computation;
  # note that hist() can fail on Byzantine cases for any of "fd", "scott",
  # "sturges", so we'll use try/catch in that order;
  # also note that in some cases "fd" freaks out -- e.g., 
  #    > set.seed(123456); vals = igamma(runif(20),0.01,scale=0.03,showHist=T)
  #    > histInfo = hist(vals,breaks="fd",plot=FALSE)
  #    > length(histInfo$breaks)
  #    [1] 275065
  # In some _really_ bizarre cases, using "fd" really-really freaks out when
  # the IQR (see http://bit.ly/2lbSN34) is extremely small.  As a specific
  # example:
  # 
  #    > n = 10; set.seed(9923527)
  #    > qvals = qgamma(runif(n), shape = 0.01, scale = 0.003)
  #    > binSize = 2 * IQR(qvals) / (10)^(1/3)
  #    > binSize
  #    [1] 7.105208e-19
  #    > numBins = (max(qvals) - min(qvals)) / binSize
  #    > numBins
  #    [1] 77600938
  # (!!)  
  # So rather than wait for hist() to try to tell us the number of $breaks,
  # let's pre-compute for "fd", and immediately move on if more than, say, 500.
  # Note, however, that for hist's "fd", when IQR(xVals) returns 0, it defaults 
  # to mad(xVals, constant = 2), and if that's 0, returns 1.  So we also need
  # to make sure that the bin size > 0 below before trying our "fd" condition.

  if (!is.null(xVals)) 
  {
    options(warn = -1)  # suppress warnings

    maxBins  <- 500
    histInfo <- NULL

    fdBinSize <- 2 * IQR(xVals) / (length(u) ^ (1/3))
    fdNumBins <- (max(xVals) - min(xVals)) / fdBinSize
    if (fdBinSize > 0 && fdNumBins <= maxBins) {
      histInfo <- tryCatch( hist(xVals, breaks = "fd", plot = FALSE),
                  error = function(err) { return(NULL) } )
    }

    # if "fd" fails, histInfo will be NULL, so try "scott"
    if (is.null(histInfo) || length(histInfo$breaks) > maxBins) {
      histInfo <- tryCatch( hist(xVals, breaks = "scott", plot = FALSE),
                    error = function(err) { return(NULL) } )
    }

    # if "scott" also fails, histInfo will be NULL, so try "sturges"
    if (is.null(histInfo)) { 
      histInfo <- tryCatch( hist(xVals, breaks = "sturges", plot = FALSE),
                    error = function(err) { 
                      print(err) 
                      stop('inorm: Error internally using hist()') } )
    }
    if (length(histInfo$breaks) > maxBins)
       stop(paste('inorm: Error using hist() -- more than', maxBins, 'bins'))

    options(warn = 1)  # reset to our inorm state: immediate warnings

    maxHistDensity <- max(histInfo$density)

  } else {

    # not plotting any variates... only the PDF and/or CDF
    maxHistDensity <- 0

  }

  # create x and y values for plotting the cdf: in some cases (e.g.,
  # gamma shape = 0.1 and scale = 0.003) using curve() to plot creates a
  # sharp disconnect; on trial and error, 1000 points in these x/y
  # values gives a smoother curve
  x        <- seq(fromX, toX, by = (toX-fromX)/1000)
  exactCDF <- pnorm(x, mean = mean, sd = sd)

  # hang on to the exact pdf, for use in max pdf plot height below and in
  # plotting the pdf...
  exactPDF <- dnorm(seq(fromX, toX, by = (toX-fromX)/100), 
                mean = mean, sd = sd)

  # if the majority of density is packed into the first histogram bin:
  # as a general attempt, set the y range max to be no bigger than 75%
  # larger than the maximum pdf value
  pdfYMax <- max(exactPDF, maxHistDensity)
  if (maxHistDensity > max(exactPDF) * 1.75) pdfYMax <- max(exactPDF) * 1.75

  # totally empty plot allows us to know in advance what the x ticks will
  # be according to R's call to pretty()... use tick info for any/all plots...
  # ylim upper is 1, unless showing pdf (at top) but not cdf...
  firstPlotYMax <- if (showCDF == FALSE && showPDF == TRUE) pdfYMax else 1
  plot(NA, NA, xlim = c(fromX, toX), ylim = c(0, firstPlotYMax),
      xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n", las = 1)
  ticks <- par("xaxp")

  # remove any tick value that R includes that's outside fromX:toX bounds
  tickVals <- seq(ticks[1], ticks[2], by = (ticks[2] - ticks[1])/ticks[3])
  if (tickVals[1] < fromX)
    tickVals <- tickVals[-1]                    # remove first
  if (tickVals[length(tickVals)] > toX)
    tickVals <- tickVals[-length(tickVals)]     # remove last

  #############################################################################

  # handle plotting of the cdf...

  if (showCDF == TRUE)
  {
    # plot the cdf (for now in lightgray)
    lines(x, exactCDF, lwd = lwd, col = "lightgray")

    # draw our own axes (R's pretty() sometimes gives a min tick or max tick
    # value that is outside the min/max quantile bounds);
    # add the vertical axis
    axis(2, las = 1)
    mtext("F(x)", side = 2, line = 3, cex = plot_cex)

    # overlay horiz axis with ticks but no line, forcing ticks/limits to respect
    # min/max quantile, even if contrary to R's pretty() decision;
    # and a 2nd axis with line but no ticks, extending thru the max/min x vals
    axis(1, at = tickVals, lwd = 0, lwd.ticks = 1, labels = TRUE)
    axis(1, at = c(fromX, toX), labels = FALSE, tcl = 0)
    mtext("x", side = 1, line = 3, cex = plot_cex)

    # draw vertical dotted lines corresponding to the min/max quantile values
    segments(fromX, 0, fromX, 1, lty = "dotted", xpd = NA)
    segments(toX,   0, toX,   1, lty = "dotted", xpd = NA)

    # draw the title
    if (showTitle) title(titleStr, cex.main = 0.975)

    # try to find appropriate x-value to draw u's and u-points...
    # by trial and error, decreasing by -0.02*plot_range seems to work...
    uXLoc <- fromX - (0.02 * (toX - fromX))

    # NOTES: on plotting histogram density rectangles in vertical stack: height
    # of pch=20 is strheight("x")*0.75*(2/3), and use "rect" of that height -- 
    # see ?points (pch 'Details' & 'pch values');
    # strheight("x") changes with different display();
    # from experimentation, it appears as though it will be OK to plot about 16
    # _full_ rect heights down, starting from the bottom of the horizontal 
    # axis, and not impede on the hist() histogram below...
    # so use this to compute the height of one "stacked rect", which will allow
    # us to plot one large rectangle representing a stack of "stacked rects",
    # such that we don't extend beyond this 16-full-heights vertical range...
    # (side note: the default R bounding box for points is a square of side 0.01
    #  inch scaled by cex: http://www.endmemo.com/program/R/pchsymbols.php)
    #
    pch20Height    <- strheight("x") * 0.75 * (2 / 3)
    maxInStack     <- 16
    maxStackHeight <- maxInStack * pch20Height

    # now draw the inverting... 
    xValsPlotted <- rep(0, length(histInfo$counts))
    for (i in 1:length(u))
    {
      # find the low limit of the bin holding xVal
      xBinIdx <- which(xVals[i] < histInfo$breaks)[1] - 1

      # draw the u-value point
      points(uXLoc, u[i], pch = 20, col = colorTrans, xpd = NA)
      points(uXLoc, u[i], pch = 21, col = colorOutline, cex = 0.8, xpd = NA)

      # draw the dashed segment from u to the cdf, and cdf to horizontal:
      if (minPlotQuantile <= u[i] && u[i] <= maxPlotQuantile) 
      {
        # if u is within min/max quantile bounds, draw horizontal and vertical
        segments(uXLoc,    u[i], xVals[i], u[i], lty = "dashed", col = color)
        segments(xVals[i], u[i], xVals[i], 0,    lty = "dashed", col = color) 
      } 
      else if (u[i] > maxPlotQuantile) 
      {
        # if u is greater than max quantile, draw horizontal to max x axis val
        segments(uXLoc, u[i], toX, u[i], lty = "dashed", col = color)
      }
      else
      {
        # if u is less than min quantile, draw horizontal to min x axis val,
        segments(uXLoc, u[i], fromX, u[i], lty = "dashed", col = color)
      }

      # for this variate value, track the number in hist bin plotted
      xValsPlotted[xBinIdx] <- xValsPlotted[xBinIdx] + 1 
      vertPos <- xValsPlotted[xBinIdx]

      # draw the variate on the line only if within min/max quantile bounds
      if (minPlotQuantile <= u[i] && u[i] <= maxPlotQuantile) 
      {
        points(xVals[i], 0, pch = 20, col = colorTrans, xpd = NA)
        points(xVals[i], 0, pch = 21, col = colorOutline, cex = 0.8, xpd = NA)
      }

      if (plotDelay > 0) Sys.sleep(plotDelay)

    } # for (i in 1:length(u))

    # redraw the cdf curve back over top of dashed variate generation lines...
    lines(x, exactCDF, lwd = lwd)

    # now draw rectangles beneath the horizontal axis showing upside-down
    # density of points; just draw a single rectange of the max stack height
    # (computed above) scaled by the histogram's bin density 
    xAxisYLoc <- -0.04
    for (i in 1:length(histInfo$density))
    {
      rectXMin    <- histInfo$breaks[i]
      rectXMax    <- histInfo$breaks[i+1]
      rectDensity <- histInfo$density[i]

      # avoid drawing if zero density or completely outside limits
      if (rectDensity == 0 || rectXMin > toX || rectXMax < fromX) next

      # don't draw beyond the lower/upper limits of the xaxis
      if (rectXMin < fromX) rectXMin <- fromX
      if (rectXMax > toX)   rectXMax <- toX

      rect(rectXMin, xAxisYLoc - (rectDensity/maxHistDensity * maxStackHeight),
           rectXMax, xAxisYLoc, border = NA, col = colorTrans, xpd = NA)
    }

    # re-draw the axis and dotted quantile lines...
    axis(1, at = tickVals, lwd = 0, lwd.ticks = 1, labels = TRUE)
    axis(1, at = c(fromX, toX), labels = FALSE, tcl = 0)
    segments(fromX, 0, fromX, 1, lty = "dotted", xpd = NA)
    segments(toX,   0, toX,   1, lty = "dotted", xpd = NA)

    # re-draw the x-axis title, as well as one "u" atop all u-points
    mtext("x", side = 1, line = 3, cex = plot_cex)
    text(uXLoc, max(u), "u", pos = 3, xpd = NA) # seems to not need plot_cex

  }  # if (showCDF == TRUE)

  #############################################################################

  # handle plotting of the pdf / histogram of variates 

  if (showPDF == TRUE)
  {
    # need a new plot if cdf took up the first empty plot
    if (showCDF == TRUE)
      plot(NA, NA, las = 1, bty = "n", 
         xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "",
         xlim = c(fromX, toX), ylim = c(0, pdfYMax))

    # draw the title, only if not already drawn by showCDF above
    if (showTitle && showCDF == FALSE) title(titleStr, cex.main = 0.975)

    if (!is.null(xVals))  # plotting variates
    {
      # draw the histogram, respecting the appropriate clip region
      clip(fromX, toX, -0.1, pdfYMax)      # use -0.1 so bottom border shows fully
      plot(histInfo, freq = FALSE, col = color, add = TRUE)
      do.call("clip", as.list(par("usr"))) # reset clip

      # for any histogram bins that are cut off by pdfYMax clipping, 
      # draw "..." above the bin indicating that it continues north
      for (i in 1:length(histInfo$density)) 
      {
        rectDensity <- histInfo$density[i]
        if (rectDensity > pdfYMax) {
          rectXMin    <- max(histInfo$breaks[i], fromX)
          rectXMax    <- min(histInfo$breaks[i+1], toX)
          rectCenter  <- rectXMin + (rectXMax - rectXMin) / 2
          text(rectCenter, pdfYMax, "...", pos = 3, srt = 90, xpd = TRUE)
        }
      }
    }

    # superimpose the theoretical (use xAxisMax here so the curve will
    # extend through to the end of the axis)
    curve(dnorm(x, mean = mean, sd = sd),
              xlim = c(fromX, toX), add = TRUE, lwd = 2)

    # add the vertical & horizontal axes
    axis(2, las = 1)
    mtext("f(x)", side = 2, line = 3, cex = plot_cex)  # if 3 plots, R lowers cex 

    axis(1, at = tickVals, lwd = 0, lwd.ticks = 1, labels = TRUE)
    axis(1, at = c(fromX, toX), labels = FALSE, tcl = 0)
    mtext("x", side = 1, line = 3, cex = plot_cex)

    # draw vertical dotted lines corresponding to the min and max quantile values
    segments(fromX, 0, fromX, pdfYMax, lty = "dotted", xpd = NA)
    segments(toX,   0, toX,   pdfYMax, lty = "dotted", xpd = NA)

  }  # if (showPDF == TRUE)

  #############################################################################

  # handle plotting of the ecdf with superimposed cdf

  if (showECDF == TRUE)
  {
    # need a new plot if either cdf or pdf took up the first empty plot
    if (showCDF == TRUE || showPDF == TRUE)
      plot(NA, NA, xlim = c(fromX,toX), ylim = c(0,1),
          xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n", las = 1)

    clip(fromX, toX, -0.1, 1.1)  # need to go lower than y=0 for the y=0 lwd

    if (is.null(xVals))
    {
      # not plotting variates
      lines(x, exactCDF, lwd = 2, col = "black", lty = "solid")
    }
    else
    {
      # plot the cdf -- ensure doesn't plot outside bounds;  draw wider 
      # so ecdf can clearly be seen overlayed, and in light gray, with
      # dashed black over top
      lwd <- 3
      cdf_lwd <- 8
      cdfColor <- "gray50"
      lines(x, exactCDF, lwd = cdf_lwd, col = cdfColor)
      lines(x, exactCDF, lwd = 1, col = "black", lty = "dashed")

      # plot the ecdf -- ensure doesn't plot outside bounds;  sometimes the
      # plot.ecdf() function doesn't want to draw the first and last horizontals
      # all the way from/to fromX/toX... so just draw it ourselves... 
      ecdfFcn   <- ecdf(xVals)
      ecdfKnots <- knots(ecdfFcn)
      plot.ecdf(ecdfFcn, verticals = TRUE, pch = "", add = TRUE,
          lwd = lwd, col = color, col.01line = NA)
      segments(fromX, 0, ecdfKnots[1], 0, lwd = lwd, col = color, lend = 1)
      segments(ecdfKnots[length(ecdfKnots)], 1, toX, 1, 
          lwd = lwd, col = color, lend = 1)
    }

    do.call("clip", as.list(par("usr"))) # reset clip

    # add the vertical axis
    axis(2, las = 1)
    mtext("F(x)", side = 2, line = 3, cex = plot_cex)

    # overlay horiz axis with ticks but no line, forcing ticks/limits to respect
    # min/max quantile, even if contrary to R's pretty() decision;
    # and a 2nd axis with line but no ticks, extending thru the max/min x vals
    axis(1, at = tickVals, lwd = 0, lwd.ticks = 1, labels = TRUE)
    axis(1, at = c(fromX, toX), labels = FALSE, tcl = 0)
    mtext("x", side = 1, line = 3, cex = plot_cex)

    # draw vertical dotted lines corresponding to the min/max quantile values
    segments(fromX, 0, fromX, 1, lty = "dotted", xpd = FALSE)
    segments(toX,   0, toX,   1, lty = "dotted", xpd = FALSE)

    # draw the title
    if (showTitle && showCDF == FALSE && showPDF == FALSE) {
      title(titleStr, cex.main = 0.975)
    }

  }  # if (showECDF == TRUE)

  #############################################################################

  # reset display, margins, and warnings
  if (!respectLayout) {
    par(mfrow = c(1, 1))
    par(mar = c(5.1, 4.1, 4.1, 2.1))
  }
  options(warn = warnVal$warn)

  # return a value/vector of F^-1(u)
  if (is.null(xVals))
    return(invisible(xVals))
  else
    return(xVals)

} # function inorm
