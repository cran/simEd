###############################################################################
# R function to visualize the binomial idf evaluated at a provided uniform(0,1)
# u. This will graph the idf in action (via dashed lines back across the cdf).
# Note that the u argument can be a scalar or vector.  If a vector, multiple
# dashed lines will be displayed, and the return type will be a vector.
# The function also gives the option of displaying a discrete histogram of the 
# variates generated, with theoretical binomial superimposed as spike/dots.
###############################################################################
ibinom <- function(u = runif(1), size, prob,
                   minPlotQuantile = 0,
                   maxPlotQuantile = 1,
                   plot            = TRUE, 
                   showCDF         = TRUE,
                   showPMF         = FALSE,
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

  if (missing(size)) stop("argument 'size' is missing, with no default")
  if (!is.numeric(size) || length(size) != 1 || size < 0 || floor(size) != size)
    stop("'size' must be a non-negative integer value")

  if (missing(prob)) stop("argument 'prob' is missing, with no default")
  if (!is.numeric(prob) || length(prob) != 1 || prob < 0 || prob > 1)
    stop("'prob' must be a numeric value in [0,1]")

  if (!is.numeric(minPlotQuantile) || length(minPlotQuantile) != 1 ||
         minPlotQuantile < 0 || minPlotQuantile >= 1)
    stop("'minPlotQuantile' must be a numeric value in [0,1)")
  if (!is.numeric(maxPlotQuantile) || length(maxPlotQuantile) != 1 ||
         maxPlotQuantile <= 0 || maxPlotQuantile > 1)
    stop("'maxPlotQuantile' must be a numeric value in (0,1]")
  if (minPlotQuantile >= maxPlotQuantile) 
    stop("'minPlotQuantile' must be less than 'maxPlotQuantile'")

  if (!is.logical(plot) || length(plot) != 1)
    stop("'plot' must be single a logical value")
  if (!is.logical(showCDF) || length(showCDF) != 1)
    stop("'showCDF' must be a single logical value")
  if (!is.logical(showPMF) || length(showPMF) != 1)
    stop("'showPMF' must be a single logical value")
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
  options(warn = 1)           # set to immediate warnings

  # compute the plot min and max
  fromX <- qbinom(minPlotQuantile, size, prob)
  toX   <- qbinom(maxPlotQuantile, size, prob)

  # check 'show' entry if not null
  if (length(show) == 3) {
    indivSum = 4*as.integer(showCDF) + 2*as.integer(showPMF) + as.integer(showECDF)
    showSum  = 4*show[1] + 2*show[2] + show[3]
    if ((!missing(showCDF) || !missing(showPMF) || !missing(showECDF))
        && indivSum != showSum)
      warning(paste("use of 'show' does not match 'showCDF','showPMF','showECDF';", 
                    "'show' values will take precedence"))
    showCDF  <- as.logical(show[1])  # a 0 or 1 entry
    showPMF  <- as.logical(show[2])
    showECDF <- as.logical(show[3])
  } else if (length(show) == 1) {
    indivSum = 4*as.integer(showCDF) + 2*as.integer(showPMF) + as.integer(showECDF)
    if ((!missing(showCDF) || !missing(showPMF) || !missing(showECDF))
        && indivSum != show)
      warning(paste("use of 'show' does not match 'showCDF','showPMF','showECDF';", 
                    "'show' values will take precedence"))
    # treat a la chmod command from Unix (where they probably do bit shifting)
    showCDF <- showPMF <- showECDF <- FALSE
    if (show >= 4) { showCDF  <- TRUE; show <- show - 4}
    if (show >= 2) { showPMF  <- TRUE; show <- show - 2}
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
    # by default, if u is null and showPMF is missing (using default FALSE),
    # let's set showPMF to TRUE -- the user can override by giving F explicitly
    if (missing(showPMF)) showPMF <- TRUE

    plotDelay <- 0
    xVals     <- NULL
  } else {
    # generate the variates
    xVals <- qbinom(u, size, prob)
  }

  #############################################################################

  # if no plot, or "equal" min/max quantile values, just return the variates...

  if (plot == FALSE)
    if (is.null(xVals)) return(invisible(xVals)) else return(xVals)

  if (showCDF == FALSE && showPMF == FALSE && showECDF == FALSE)
  {
    if (plot == TRUE) 
      warning("ignoring plot since showCDF, showPMF, and showECDF are all FALSE")
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
  minPlots  <- sum(showCDF, showPMF, showECDF)

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
  par(mar = c(botMar, 4.1, 2.1, 1.5))   # 0.5 for right??

  # set color for plotting variates
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
  titleStr <- paste("binomial(size = ", as.character(size),
              ", prob = ", as.character(prob), ")\n", sep = "")
  if (!is.null(xVals))
    titleStr <- paste(titleStr, "random variate generation", sep = "")

  # for plotting, if quantiles are not 0 and/or 1, we need to plot just
  # slightly to the left/right to make the discrete stairstep cdf look right...
  xAxisMin <- fromX
  xAxisMax <- toX
  if (fromX > 0)    xAxisMin <- fromX - (0.02 * (toX - fromX))
  if (toX   < size) xAxisMax <- toX   + (0.02 * (toX - fromX))

  # hang on to the exact pmf and the estimated pmf (via table), for use in max 
  # pmf plot height computed below and in plotting the pmf...
  exactPMF <- dbinom(fromX:toX, size, prob)
  exactCDF <- pbinom((0:size), size, prob)

  if (!is.null(xVals)) 
  {
    estimPMF <- table(xVals) / length(xVals)
    # as a general attempt, set the y range max to be no bigger than 75%
    # larger than the maximum pmf value
    pmfYMax <- max(exactPMF, estimPMF)
    if (max(estimPMF) > max(exactPMF) * 1.5) pmfYMax <- max(exactPMF) * 1.5
  } 
  else 
  {
    estimPMF <- NULL
    pmfYMax <- max(exactPMF)
  }

  # totally empty plot allows us to know in advance what the x ticks will
  # be according to R's call to pretty()... use tick info for any/all plots...
  # ylim upper is 1, unless showing pmf (at top) but not cdf...
  firstPlotYMax <- if (showCDF == FALSE && showPMF == TRUE) pmfYMax else 1
  plot(NA, NA, xlim = c(xAxisMin, xAxisMax), ylim = c(0, firstPlotYMax),
      xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n", las = 1)
  ticks <- par("xaxp")

  # remove any tick value that R includes that's outside fromX:toX bounds
  tickVals <- seq(ticks[1], ticks[2], by = ((ticks[2] - ticks[1])/ticks[3]))
  if (ticks[1] < fromX) { tickVals <- tickVals[-1] }
  if (ticks[2] > toX)   { tickVals <- tickVals[-length(tickVals)] }
  if (toX - fromX <= 5) { tickVals <- fromX:toX }  # avoid R's decimals...

  #############################################################################

  # handle plotting of the cdf...

  if (showCDF == TRUE)
  {
    # plot the cdf (for now in lightgray)
    lines(fromX:toX, exactCDF[(fromX:toX) + 1], type = "s", 
        lwd = lwd, col = "lightgray")

    # plotting the discrete cdf using "s" omits the first vertical -- draw it
    fromY <- if (fromX == 0) 0 else exactCDF[fromX]
    segments(fromX, fromY, fromX, exactCDF[fromX + 1], lwd = lwd,
          col = "lightgray")

    # when minPlotQuantile causes fromX > 0, draw incoming stub of horizontal
    # and intersecting dotted vertical 
    if (fromX > 0) {
      segments(xAxisMin, exactCDF[fromX], fromX, exactCDF[fromX], lwd = lwd,
          col = "lightgray")
      segments(xAxisMin, 0, xAxisMin, 1, lty = "dotted", xpd = NA)
    }

    # when maxPlotQuantile causes toX < size, draw outgoing stub of horizontal
    # and intersecting dotted vertical 
    if (toX < size) {
      segments(toX, exactCDF[toX + 1], xAxisMax, exactCDF[toX + 1], lwd = lwd,
          col = "lightgray")
      segments(xAxisMax, 0, xAxisMax, 1, lty = "dotted", xpd = NA)
    }

    # but draw our own axes (R's pretty() sometimes gives a min tick or max tick
    # value that is outside the min/max quantile bounds);
    # add the vertical axis
    axis(2, las = 1)
    mtext("F(x)", side = 2, line = 3, cex = plot_cex)

    # overlay horiz axis with ticks but no line, forcing ticks/limits to respect
    # min/max quantile, even if contrary to R's pretty() decision;
    # and a 2nd axis with line but no ticks, extending thru the max/min x vals
    axis(1, at = tickVals, lwd = 0, lwd.ticks = 1, labels = TRUE)
    axis(1, at = c(xAxisMin, xAxisMax), labels = FALSE, tcl = 0)
    mtext("x", side = 1, line = 3, cex = plot_cex)

    # draw the title
    if (showTitle) title(titleStr, cex.main = 0.975)

    # try to find appropriate x-value to draw u's and u-points...
    # by trial and error, decreasing by -0.02*plot_range seems to work...
    uXLoc <- xAxisMin - (0.02 * (xAxisMax - xAxisMin))

    # NOTES: on plotting generated variates in a vertical stack: height of 
    # pch=20 is strheight("x")*0.75*(2/3) -- see ?points (pch 'Details' & 
    # 'pch values');
    # strheight("x") changes with different display();
    # from experimentation, it appears as though it will be OK to plot about 18 
    # of these _full_ point heights down, starting from the bottom of the
    # horizontal axis, and not impede on the histogram below...
    # so use this to compute the stack height, which will allow us to plot 
    # points on top of one another if necessary so that we don't extend beyond 
    # this 18-full-heights vertical range...
    # (side note: the default R bounding box for points is a square of side 0.01 
    #  inch scaled by cex: http://www.endmemo.com/program/R/pchsymbols.php)
    #
    pch20Height   <- strheight("x") * 0.75 * (2 / 3)
    maxInStack    <- 18
    maxXValsCount <- max(table(xVals))      # max count (for scaling vert stacking)
    vertOffset    <- pch20Height
    if (maxXValsCount > maxInStack) { 
      maxStackHeight <- maxInStack * pch20Height
      vertOffset <- maxStackHeight / maxXValsCount
    }

    # now draw the inverting... 
    xValsPlotted <- rep(0, size + 1)        # size + 1 distinct x values
    for (i in 1:length(u))
    {
      xValIdx <- xVals[i] + 1  # to handle 0-variate's index in R

      # draw the u value point
      points(uXLoc, u[i], pch = 20, col = colorTrans, xpd = NA)
      points(uXLoc, u[i], pch = 21, col = colorOutline, cex = 0.8, xpd = NA)

      # draw the dashed segment from u to the cdf;  unlike continuous case,
      # have to check fromX / xVals[i] / toX rather than quantiles because
      # many quantiles map to same xVals[i]
      if (fromX <= xVals[i] && xVals[i] <= toX)
      {
        segments(uXLoc, u[i], xVals[i], u[i], lty = "dashed", col = color)
        # draw the dashed segment from cdf to horizontal only once;
        # otherwise, draw just to the bottom of the cdf riser 
        if (xValsPlotted[xValIdx] == 0) {
            segments(xVals[i], u[i], xVals[i], 0, lty = "dashed", col = color) 
        } else {
            segments(xVals[i], u[i], xVals[i], exactCDF[max(1, xValIdx - 1)], 
              lty = "dashed", col = color)
        }
      }
      else if (xVals[i] > toX)
      {
        # if u is greater than max quantile, draw horizontal to max x axis val
        segments(uXLoc, u[i], xAxisMax, u[i], lty = "dashed", col = color)
      }
      else
      {
        # if u is less than min quantile, draw horizontal to min x axis val,
        segments(uXLoc, u[i], xAxisMin, u[i], lty = "dashed", col = color)
      }

      # for this variate value, track the number plotted
      xValsPlotted[xValIdx] <- xValsPlotted[xValIdx] + 1 
      vertPos <- xValsPlotted[xValIdx]

      # draw the variate in a vertical stack only if w/in fromX/toX bounds
      if (fromX <= xVals[i] && xVals[i] <= toX)
      {
        yVal <- 0 - (vertOffset * (vertPos-1))
        points(xVals[i], yVal, pch = 20, col = colorTrans, xpd = NA)
        points(xVals[i], yVal, pch = 21, col = colorOutline, cex = 0.8, xpd = NA)
      }

      if (plotDelay > 0) Sys.sleep(plotDelay)

    } # for (i in 1:length(u))

    # redraw the cdf back over top of dashed variate generation lines...
    lines(fromX:toX, exactCDF[(fromX:toX) + 1], type = "s", lwd = lwd)

    # re-draw axis, 1st vertical, cdf "extensions", & dotted quantile lines...
    axis(1, at = tickVals, lwd = 0, lwd.ticks = 1, labels = TRUE)
    axis(1, at = c(xAxisMin,xAxisMax), labels = FALSE, tcl = 0)
    segments(fromX, fromY, fromX, exactCDF[fromX + 1], lwd = lwd)
    if (fromX > 0) {
      segments(xAxisMin, exactCDF[fromX], fromX, exactCDF[fromX], lwd = lwd)
      segments(xAxisMin, 0, xAxisMin, 1, lty = "dotted", xpd = NA)
    }
    if (toX < size) {
      segments(toX, exactCDF[toX + 1], xAxisMax, exactCDF[toX + 1], lwd = lwd)
      segments(xAxisMax, 0, xAxisMax, 1, lty = "dotted", xpd = NA)
    }

    # re-draw the x-axis title, as well as one "u" atop all u-points
    mtext("x", side = 1, line = 3, cex = plot_cex)
    text(uXLoc, max(u), "u", pos = 3, xpd = NA) # seems to not need plot_cex

  }  # if (showCDF == TRUE)

  #############################################################################

  # handle plotting of the pmf / histogram of variates 

  if (showPMF == TRUE)
  {
    # need a new plot if cdf took up the first empty plot
    if (showCDF == TRUE)
      plot(NA, NA, xlim = c(xAxisMin, xAxisMax), ylim = c(0, pmfYMax),
          xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n", las = 1)

    # draw the title, only if not already drawn by showCDF above;
    if (showTitle && showCDF == FALSE) title(titleStr, cex.main = 0.975)

    # plot the theoretical -- start w/ black spikes behind, adding points below
    points(fromX:toX, exactPMF, pch = "", type = "h")

    if (!is.null(xVals))  # no variates to plot
    {
      # overlay the estimated pmf using table w/ type='h' (discrete);  
      # NB: (a) we want to avoid printing outside of the [fromX, toX] bounds,
      #     (b) table does not necessarily create an entry per x value, 
      #     (c) access the count of a table value a la estim["27"] not estim[27]
      #     (d) "..." doesn't center above the lwd=3 line, so need an adjustment
      #     (e) "..." can push on title when PMF is 1st plot, so stop drawing short
      for (xVal in fromX:toX) 
      {
        xProb <- estimPMF[as.character(xVal)]
        if (!is.na(xProb))  # no xVal entry will return NA
        {
          if (xProb > pmfYMax) {
             points(xVal, pmfYMax * 0.95, type = "h", col = color, lwd = 3)
             # put "..." above if height is cut off by plot max
             text(xVal, pmfYMax * 0.95, labels = "...", srt = 90, 
                  adj = c(-0.2, 0.075), xpd = TRUE, cex = 1.25)
          } else {
             points(xVal, xProb, type = "h", col = color, lwd = 3)
          }
        }
      }
    }

    # superimpose the theoretical points
    points(fromX:toX, exactPMF, pch = 20)

    # add the vertical & horizontal axes
    axis(2, las = 1)
    mtext("f(x)", side = 2, line = 3, cex = plot_cex)  # if 3 plots, R lowers cex 

    axis(1, at = tickVals, lwd = 0, lwd.ticks = 1, labels = TRUE)
    axis(1, at = c(xAxisMin, xAxisMax), labels = FALSE, tcl = 0)
    mtext("x", side = 1, line = 3, cex = plot_cex)

    # draw vertical dotted lines corresponding to the min and max quantile values
    if (fromX > 0) 
      segments(xAxisMin, 0, xAxisMin, pmfYMax, lty = "dotted", xpd = TRUE)
    if (toX < size)
      segments(xAxisMax, 0, xAxisMax, pmfYMax, lty = "dotted", xpd = TRUE)

  }  # if (showPMF == TRUE)

  #############################################################################

  # handle plotting of the ecdf with superimposed cdf

  if (showECDF == TRUE)
  {
    # need a new plot if either cdf or pdf took up the first empty plot
    if (showCDF == TRUE || showPMF == TRUE)
      plot(NA, NA, xlim = c(xAxisMin, xAxisMax), ylim = c(0, 1),
          xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n", las = 1)

    clip(xAxisMin, xAxisMax, -0.1, 1.1)  # need to go lower than y=0 for y=0 lwd

    if (is.null(xVals))
    {
      # not plotting variates
      lines(fromX:toX, exactCDF[(fromX:toX) + 1], type = "s", 
         lwd = 2, lty = "solid", col = "black")
      
      # draw 1st vertical, cdf "extensions", & dotted quantile lines...
      fromY <- if (fromX == 0) 0 else exactCDF[fromX]
      segments(fromX, fromY, fromX, exactCDF[fromX + 1], 
          lwd = 2, col = "black", lty = "solid", xpd = NA)
      if (fromX > 0) {
        segments(xAxisMin, exactCDF[fromX], fromX, exactCDF[fromX], lwd = 2)
        segments(xAxisMin, 0, xAxisMin, 1, lty = "dotted", xpd = NA)
      }
      if (toX < size) {
        segments(toX, exactCDF[toX + 1], xAxisMax, exactCDF[toX + 1], lwd = 2)
        segments(xAxisMax, 0, xAxisMax, 1, lty = "dotted", xpd = NA)
      }
    }
    else
    {
      # plot the cdf -- ensure doesn't plot outside bounds;  draw wider 
      # so ecdf can clearly be seen overlayed, and in light gray, with
      # dashed black over top
      lwd <- 3
      cdf_lwd <- 8
      cdfColor <- "gray50"
      x <- NULL
      lines(fromX:toX, exactCDF[(fromX:toX) + 1], type = "s", 
         lwd = cdf_lwd, col = cdfColor)
      lines(fromX:toX, exactCDF[(fromX:toX) + 1], type = "s", 
         lwd = 1, col = "black", lty = "dashed")

      # draw 1st vertical, cdf "extensions", & dotted quantile lines...
      fromY <- if (fromX == 0) 0 else exactCDF[fromX]
      segments(fromX, fromY, fromX, exactCDF[fromX + 1], 
          lwd = cdf_lwd, col = cdfColor, xpd = NA)
      segments(fromX, fromY, fromX, exactCDF[fromX + 1], 
          lwd = 1, col = "black", lty = "dashed", xpd = NA)
      if (fromX > 0) {
        clip(xAxisMin, xAxisMax, -0.1, 1.1)
        segments(xAxisMin, exactCDF[fromX], fromX, exactCDF[fromX], 
          lwd = cdf_lwd, col = cdfColor, xpd = FALSE)
        segments(xAxisMin, 0, xAxisMin, 1, lty = "dotted", xpd = NA)
      }
      if (toX < size) {
        clip(xAxisMin, xAxisMax, -0.1, 1.1)
        segments(toX, exactCDF[toX + 1], xAxisMax, exactCDF[toX + 1],
            lwd = cdf_lwd, col = cdfColor, xpd = FALSE)
        segments(toX, exactCDF[toX + 1], xAxisMax, exactCDF[toX + 1],
            lwd = 1, col = "black", lty = "dashed", xpd = NA)
        segments(xAxisMax, 0, xAxisMax, 1, lty = "dotted", xpd = NA)
      }

      # according to R documentation, different plots can react differently
      # to clip... seems we need to call again here for plot.ecdf to respect
      clip(xAxisMin, xAxisMax, -0.1, 1.1)

      # plot the ecdf -- ensure doesn't plot outside bounds;  sometimes the
      # plot.ecdf() function doesn't want to draw the first and last horizontals
      # all the way from/to fromX/toX... so just draw it ourselves... 
      ecdfVals    <- ecdf(xVals)         # a function!
      ecdfKnots   <- knots(ecdfVals)
      firstKnot   <- ecdfKnots[1]
      lastKnotIdx <- length(ecdfKnots)
      lastKnot    <- ecdfKnots[lastKnotIdx]
      plot.ecdf(ecdfVals, verticals = TRUE, pch = "", add = TRUE,
          lwd = lwd, col = color, col.01line = NA)
      segments(0, 0, firstKnot, 0, lwd = lwd, col = color)
      segments(lastKnot, 1, size, 1, lwd = lwd, col = color)

      # handle some special cases where either vertical lines may not appear
      if (firstKnot == 0 && fromX == 0)
      {
        # when ecdfVals(0) == 1, plot.ecdf doesn't give us the 0-to-1 riser, 
        # so draw it; otherwise, first vertical cut by clip -- draw w/ xpd=NA
        nextHeight <- if (ecdfVals(0) == 1) 1 else ecdfVals(0)
        segments(0, 0, 0, nextHeight, lwd = lwd, col = color, xpd = NA)
      }
      if (lastKnot == size) 
      {
        # the very last vertical is getting cut by clip -- draw it with xpd=NA
        prevHeight <- 0
        if (lastKnotIdx > 1) prevHeight <- ecdfVals(ecdfKnots[lastKnotIdx - 1])
        segments(size, prevHeight, size, 1, lwd = lwd, col = color, xpd = NA)
      }

      do.call("clip", as.list(par("usr"))) # reset clip

    } # else (!is.null(xVals))

    # add the vertical axis
    axis(2, las = 1)
    mtext("F(x)", side = 2, line = 3, cex = plot_cex)

    # overlay horiz axis with ticks but no line, forcing ticks/limits to respect
    # min/max quantile, even if contrary to R's pretty() decision;
    # and a 2nd axis with line but no ticks, extending thru the max/min x vals
    axis(1, at = tickVals, lwd = 0, lwd.ticks = 1, labels = TRUE)
    axis(1, at = c(xAxisMin, xAxisMax), labels = FALSE, tcl = 0)
    mtext("x", side = 1, line = 3, cex = plot_cex)

    if (fromX > 0)  segments(xAxisMin, 0, xAxisMin, 1, lty = "dotted", xpd = NA)
    if (toX < size) segments(xAxisMax, 0, xAxisMax, 1, lty = "dotted", xpd = NA)

    # draw the title
    if (showTitle && showCDF == FALSE && showPMF == FALSE) {
      title(titleStr, cex.main = 0.975)
    }

  } # if (showECDF == TRUE)

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

} # function ibinom
