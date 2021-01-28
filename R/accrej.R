## -------------------------------------------------------------------------
#' Acceptance-Rejection Algorithm Visualization
#'
#' @description This function animates the process of generating variates via
#'          acceptance-rejection for a specified density function (pdf) bounded
#'          by a specified majorizing function.
#'        
#' @param n           number of variates to generate.
#' @param pdf         desired probability density function from which random
#'                    variates are to be drawn
#' @param support     the lower and upper bounds of the support of the
#'                    probability distribution of interest, specified as a
#'                    two-element vector.
#' @param majorizingFcn     majorizing function.  Default value is NULL,
#'                          corresponding to a constant majorizing function that is
#'                          1.01 times the maximum value of the pdf. May
#'                          alternatively be provided as a user-specified function,
#'                          or as a data frame requiring additional notation as 
#'                          either piecewise-constant or piecewise-linear.  See
#'                          examples.
#' @param majorizingFcnType used to indicate whether a majorizing function that
#'                          is provided via data frame is to be interpreted as
#'                          either piecewise-constant (\code{"pwc"}) or
#'                          piecewise-linear (\code{"pwl"}).  If the majorizing
#'                          function is either the default or a user-specified
#'                          function (closure), the value of this parameter is
#'                          ignored.
#' @param seed              initial seed for the uniform variates used during generation.
#' 
#' @param maxTrials         maximum number of accept-reject trials; infinite by default
#' @param plot              if TRUE, visual display will be produced.  If FALSE,
#'                          generated variates will be returned without visual display.
#' @param showTitle         if TRUE, display title in the main plot.
#' @param plotDelay         wait time, in seconds, between plots; -1 (default) for interactive
#'                             mode, where the user is queried for input to progress.
#'
#' @details  There are three modes for visualizing the acceptance-rejection 
#'            algorithm for generating random variates from a particular 
#'            probability distribution:  
#'            \itemize{
#'               \item interactive advance (\code{plotDelay = -1}), where 
#'                  pressing the 'ENTER' key advances to the next step
#'                  (an accepted random variate) in the algorithm, 
#'                  typing 'j #' jumps ahead # steps,
#'                  typing 'q' quits immediately,
#'                  and typing 'e' proceeds to the end; 
#'               \item automatic advance (\code{plotDelay} > 0); or
#'               \item final visualization only (\code{plotDelay = 0}).
#'            }
#'            As an alternative to visualizing, variates can be generated
#             when \code{plot = FALSE}.
#'
#' @return   Returns the n generated variates accepted.
#'
#' @concept  random variate generation
#' 
#' @importFrom shape Arrows
#' @importFrom grDevices dev.list dev.new adjustcolor
#' @importFrom stats integrate uniroot
#'
#' @examples
#'
#' accrej(n = 20, seed = 8675309, plotDelay = 0)
#' \dontrun{
#' accrej(n = 10, seed = 8675309, plotDelay = 0.1)
#' accrej(n = 10, seed = 8675309, plotDelay = -1)
#'
#' # Piecewise-constant majorizing function
#' m <- function(x) {
#'   if      (x < 0.3)  1.0 
#'   else if (x < 0.85) 2.5
#'   else               1.5
#' }
#' accrej(n = 100, seed = 8675309, majorizingFcn = m, plotDelay = 0)
#'
#' # Piecewise-constant majorizing function as data frame
#' m <- data.frame(
#'   x = c(0.0, 0.3, 0.85, 1.0),
#'   y = c(1.0, 1.0, 2.5,  1.5))
#' accrej(n = 100, seed = 8675309, majorizingFcn = m, 
#'        majorizingFcnType = "pwc", plotDelay = 0)
#'
#' # Piecewise-linear majorizing function as data frame
#' m <- data.frame(
#'    x = c(0.0, 0.1, 0.3, 0.5, 0.7, 1.0), 
#'    y = c(0.0, 0.5, 1.1, 2.2, 1.9, 1.0))
#' accrej(n = 100, seed = 8675309, majorizingFcn = m, 
#'        majorizingFcnType = "pwl", plotDelay = 0)
#' 
#' # invalid majorizing function; should give warning
#' accrej(n = 20, majorizingFcn = function(x) dbeta(x, 1, 3), plotDelay = 0)
#' }
#' 
#' # Piecewise-linear majorizing function with power-distribution density function
#' m <- data.frame(x = c(0, 1, 2), y = c(0, 0.375, 1.5))
#' samples <- accrej(n = 100, pdf = function(x) (3 / 8) * x ^ 2, support = c(0,2),
#'                   majorizingFcn = m, majorizingFcnType = "pwl", plotDelay = 0)
#'
#' @export
################################################################################
accrej <- function(
                n                 = 20,
                pdf               = function(x) dbeta(x, 3, 2),
                majorizingFcn     = NULL,
                majorizingFcnType = NULL,
                support           = c(0,1),
                seed              = NA,
                maxTrials         = Inf,
                plot              = TRUE, 
                showTitle         = TRUE,
                plotDelay         = plot * -1
                #fontScaleRatio   = c(1, 3)   # save for future release
          )
{

  #############################################################################
  # Do parameter checking and handling; stop execution or warn if erroneous
  #############################################################################
  checkVal(seed, "i", min = 1, na = TRUE, null = TRUE)
  checkVal(n, "i", min = 1)
  
  checkVal(pdf, "f")

  checkVal(maxTrials, "i", min = 1)
  checkVal(plot, "l")
  checkVal(showTitle, "l")

  if (!isValNum(plotDelay) || (plotDelay < 0 && plotDelay != -1))
    stop("'plotDelay' must be a numeric value (in secs) >= 0 or -1 (interactive mode)")
  
  #if (any(is.na(fontScaleRatio)) || length(fontScaleRatio) < 2) {
  #  stop("fontScaleRatio must be a list of two values")
  #}

  #############################################################################
  
  # Diagnostic boxed to show range of subplot windows
  showBoxes = FALSE
  
  plotxvals <- seq(support[1], support[2], length=400)
  
  if(is.null(majorizingFcn)) {
    # Override majorizing function if NA
    majorizingFcn <- function(x) 1.01 * max(sapply(plotxvals, pdf))
    useDefaultMajorizingFcn <- TRUE
  } else {
    # checkMajorizing (utils.R) also checks "pwc" or "pwl" for type
    pieces        <- checkMajorizing(majorizingFcn, majorizingFcnType, support)
    majorizingFcn <- pieces$majorizingFcn
    inversionFcn  <- pieces$inversionFcn
    mappingFcn    <- pieces$mappingFcn  # NA if type != "pwl"
    pwl_xvals     <- pieces$pwl_xvals   # NA if type != "pwl"
    pwl_yvals     <- pieces$pwl_yvals   # NA if type != "pwl"
    useDefaultMajorizingFcn <- FALSE
  }

  # Maximum function of pdf, majorizing function, and both
  # Y components of plots
  a.ys <- sapply(plotxvals, pdf)
  if (typeof(majorizingFcn) == "closure") {
    m.ys <- sapply(plotxvals, majorizingFcn)
  } else {
    m.ys <- sapply(plotxvals, mappingFcn)
  }
  
  if (plot) {
    a.maxv <- max(a.ys)
    m.maxv <- max(m.ys)
    maxv   <- max(m.maxv, a.maxv)
  }
  
  if (min(m.ys - a.ys) < 0)
    message(" - Majorizing function is not always greater than actual function")
  
  # Creating global instance of PausePlot. To be overridden in main
  PauseCurrPlot <- function() return(NA)
  
  #sf <- function(n) ScaleFont(n, f = 2, r = fontScaleRatio)   # font-scaling function
  sf <- function(n) ScaleFont(n, f = 2, r = c(1,3))   # font-scaling function

  # Construct title for the plot
  titleText <- "Acceptance-Rejection Algorithm";

  ##############################################################################
  ##  Define graphical components
  ##############################################################################
  # Ranges for Plots (Keep bounds in (10, 190))
  # -------------   xmin, xmax, ymin, ymax ------------
  mplotRange    <- c( 20,  180,   70,  140)     # Main Plot range
  histplotRange <- c( 20,  180,    5,   75)     # Histogram Plot Range
  varplotRange  <- c( 20,  180,  140,  170)     # Variable Plot Range
  
  overplotRange <- c(
    min(mplotRange[1], histplotRange[1], varplotRange[1]),
    max(mplotRange[2], histplotRange[2], varplotRange[2]),
    min(mplotRange[3], histplotRange[3], varplotRange[3]),
    max(mplotRange[4], histplotRange[4], varplotRange[4])
  ) + c(-5,5,-5,5)
  ##############################################################################


  ##############################################################################
  ### --------------   BEGIN FUNCITON DEFINITIONS FOR MAIN   --------------  ###
  ##############################################################################


  DrawARPlot <- function(currPoint, accepted, AccXs, AccYs, RejXs, RejYs) 
  {
    cbRange <- mplotRange + c(-5, 5, 2, 5) 
        # Range displacement for background components
    ScalePlots(mplotRange)

    # Draw border around region
    if (showBoxes) {
      TogglePlot(cbRange)
      DrawBorder("grey", "white")
    }
    
    # # Initialize main plot
    TogglePlot(mplotRange, initPlot = FALSE, mar = c(3,3,1,1))
    
    #plot(plotxvals, m.ys, ylab="density", ylim = c(0, maxv), 
    #     type ="l", bty = "n", col="red3", las = TRUE)
    #lines(plotxvals, a.ys, ylab="density", type ="l", col="green4")

    plot(plotxvals, a.ys, ylab = "density", type = "l", col = "green4",
        bty = "n", las = 1, xlim = support, ylim = c(0, maxv), xaxt = "n")
    axis(1, lwd = 0, lwd.ticks = 1)
    axis(1, labels = FALSE, lwd = 1, lwd.ticks = 0, at = support)

    if (typeof(majorizingFcn) == "closure") 
    {
        points(plotxvals, m.ys, type = "l", col = "red3")
        text(support[2] + (support[2] - support[1]) * 0.04, 
             majorizingFcn(max(plotxvals)), bquote(italic("f")~"*"), 
             col = "red3", xpd = NA)
    } 
    else 
    {
        # if piecewise, R's plotting draws verticals on a slight lean, so
        # let's draw as segments instead
        majorizingFcnX <- majorizingFcn[[1]]
        majorizingFcnY <- majorizingFcn[[2]]
        if (majorizingFcnType == "pwc") {
            for (i in 2:length(majorizingFcnX)) {
                segments(majorizingFcnX[i-1], majorizingFcnY[i-1],
                         majorizingFcnX[i-1], majorizingFcnY[i], col = "red")
                segments(majorizingFcnX[i-1], majorizingFcnY[i],
                         majorizingFcnX[i],   majorizingFcnY[i], col = "red")
            }
        } else {
            for (i in 2:length(majorizingFcnX)) {
                segments(majorizingFcnX[i-1], majorizingFcnY[i-1],
                         majorizingFcnX[i],   majorizingFcnY[i], col = "red")
            }
        }
        text(support[2] + (support[2] - support[1])*0.04, 
            mappingFcn(max(plotxvals)), bquote(italic("f")~"*"), 
            col = "red3", xpd = NA)
    }

    points(AccXs, AccYs, col = "green4")
    points(RejXs, RejYs, col = "red3", pch = 4)
    
    # y0 will be the majorizing function's values (m.ys) at the
    # corresponding x value... this should eventually take into
    # account the x-range (assume range is (0,1))
    y0 = round(currPoint[1] * length(m.ys))
    #segments(currPoint[1], m.ys[y0], y1 = 0, lty = "dashed")
    #segments(currPoint[1], currPoint[2], x1 = 0, lty = "dashed")
    segments(x0 = support[1],   y0 = currPoint[2], 
             x1 = currPoint[1], y1 = currPoint[2], lty = "dashed")
    segments(x0 = currPoint[1], y0 = 0, 
             x1 = currPoint[1], y1 = currPoint[2], lty = "dashed")
    
    points(currPoint[1], currPoint[2], pch = 19, cex = 1.5,
        col = if(accepted) "green4" else "red3")
    
    TogglePlot(mplotRange + c(-5, -5, 10, 0))
    if (showBoxes) DrawBorder("orange")
  
    sizeS_ <- if (Sys.getenv("RSTUDIO") == 1) sf(10) else sf(20)
    TextBox(
      text = paste(format(round(currPoint[2], 3), nsmall = 3)),
      mw = 0, mh = 15 + 175 * currPoint[2]/maxv,
      hw = 12, hh = 15,
      bg = "orange",
      #size = sf(20)
      size = sizeS_
    )
    
    TogglePlot(mplotRange + c(10, 1, 2, -5))
    if (showBoxes) DrawBorder("yellow")
    
    xloc <- 12 + (176 / diff(support)) * (currPoint[1] - support[1])
    TextBox(
      #text = paste(round(currPoint[1], 2)),
      #mw = 12 + 176 * currPoint[1]/diff(support),
      text = paste(format(round(currPoint[1], 3), nsmall = 3)),
      mw = xloc,
      mh = 0,
      hw = 12, hh = 15,
      bg = "yellow",
      #size = sf(20)
      size = sizeS_
    )
    
  }
  ##############################################################################

  ##############################################################################
  ## DrawEquation
  ## --------------------------------------------------------------------------
  ## Draw the relationship equations based on current state
  ## @param x_idx   The index of the previous x (0-base)
  ## @param xc      The previous x that was generated
  ## @param xn      The latest x to be generated
  ##############################################################################
  DrawHistPlot <- function(currPoint, accepted, Xs, Ys)
  {
    ebRange  <- histplotRange + c(-5, 5, 0, 5)

    ScalePlots(histplotRange)
    
    # Toggle to subplot and draw border 
    if (showBoxes) {
      TogglePlot(ebRange)
      DrawBorder("grey", "white")
    }

    # Toggle to main plot and initiate coordinate-bound plot
    TogglePlot(histplotRange, initPlot = FALSE, mar = c(3,3,1,1))
    
    if (length(Xs) == 0) {
        h_vals <- c(a.maxv)
        plot(NULL, xlim = c(plotxvals[1], plotxvals[length(plotxvals)]), 
                ylim = c(0, a.maxv), las = TRUE, bty = "n", xaxt = "n")
        axis(1, lwd = 0, lwd.ticks = 1)
        axis(1, labels = FALSE, lwd = 1, lwd.ticks = 0, at = support)
        # bgl: 16 Jul 2020
        # this segment draws in the bottom graph
        segments(currPoint[1], a.maxv * 1.1, y1 = 0, lty = 2, 
            col = if (accepted) "green4" else "red3")
    } else {
        if (length(Xs) > 1)
            h_vals <- hist(Xs, plot = FALSE, breaks = "fd")
        else 
            h_vals <- hist(Xs, plot = FALSE)
        hv_max <- max(h_vals$density, a.maxv)

        hist(Xs, freq = FALSE, las = TRUE, bty = "n", main = "", 
            xlim = c(plotxvals[1], plotxvals[length(plotxvals)]),
            ylim = c(0, hv_max),
            breaks = if (length(Xs) > 1) "fd" else "Sturges",
            xaxt = "n")
        axis(1, lwd = 0, lwd.ticks = 1)
        axis(1, labels = FALSE, lwd = 1, lwd.ticks = 0, at = support)
        # bgl: 16 Jul 2020
        # this segment draws in the bottom graph
        segments(currPoint[1], hv_max * 1.1, y1 = 0, lty = 2, 
            col = if (accepted) "green4" else "red3")
    }
    
    lines(plotxvals, pdf(plotxvals), ylab="density", 
          type ="l", col = adjustcolor("green4", alpha.f=1.0), lwd = 1.5)
  }

  ##############################################################################

  ##############################################################################
  ##  DrawLinePlot (and associated function-scope holders)
  ## --------------------------------------------------------------------------
  ##  Initialized the generated value plot depending on specifications
  ##  @param xn      Point just generated; flashes as light blue
  ##  @param period  How long should the generated list be
  ##############################################################################
  DrawVarMap <- function(vpair, numAcc, numRej)
  {
    varplotRange <- varplotRange + c(-5, 5, 0,0)  # Extends a bit to go with others

    # Scale and toggle to the plot range and proceed to plot as needed
    ScalePlots(varplotRange)

    TogglePlot(varplotRange)
    DrawBorder("grey", "white")
    
    TogglePlot(varplotRange, initPlot = FALSE, mar = c(0,0,0,0))
    
    plot(NA, NA, xlim = c(0,200), ylim = c(0,200), xaxt = "n", yaxt = "n",
         xlab = "", ylab = "", bty = "n", las = 1, type = "s")
    # Special plot call to use 's' option
   
    sizeS_ <- if (Sys.getenv("RSTUDIO") == 1) sf(10) else sf(20)
    sizeL_ <- if (Sys.getenv("RSTUDIO") == 1) sf(16) else sf(30)
    TextBox(
      mw = 25, mh = 100,
      hw = 25, hh = 100,
      bg = "cadetblue1"
    )
     
    TextBox(
      text = paste("Trial", numAcc + numRej),
      mw = 25, mh = 140,
      hw = 25, hh =  40,
      #size = sf(30),
      size = sizeL_,
      border = NA
    )
    
    TextBox(
      text = paste("Accept", numAcc),
      mw = 25, mh =  75,
      hw = 20, hh =  25,
      #size = sf(20),
      size = sizeS_,
      col  = "green4",
      bg   = "white"
    )
    TextBox(
      text = paste("Reject", numRej),
      mw = 25, mh =  25,
      hw = 20, hh =  25,
      #size = sf(20),
      size = sizeS_,
      col  = "red3",
      bg   = "white"
    )
    
    TextBox(
      # bgl: 16 Jul 2020
      #text = bquote(U[1] (.(support[1]),.(support[2]))),
      # see ?plotmath
      text = bquote(u[1]~phantom(.)%~% ~~ frac(italic(f)~"*"~(italic(x)) , integral(~~italic(f)~"*"~(italic(x))*italic(dx), -infinity, infinity))),
      #text = bquote(u[1]~phantom(.)%~% ~~ frac(gamma,Gamma) ~~ ~~ ~~ ~~ list(gamma == italic(f)~"*"~(italic(x)) , ~~Gamma == integral(italic(f)~"*"~(italic(x))*italic(dx), -infinity, infinity))),
      mw = 100, mh = 150,
      hw =  50, hh =  50,
      #size = sf(20)
      size = sizeS_
    )
    
    TextBox(
      # bgl: 16 Jul 2020
      #text = bquote(U[2](0, mf(u[1]))),
      text = bquote(u[2]~phantom(.)%~% ~~ U(0, italic(f)~"*"~(u[1]))),
      mw = 100, mh = 50,
      hw =  50, hh = 30,
      #size = sf(20)
      size = sizeS_
    )
   
    Arrows(50, 100, 140, 100)
    
    u1_ <- if (is.na(vpair[1])) "..." else format(round(vpair[1],3), nsmall = 3)
    TextBox(
      text = bquote(u[1] ~"="~ .(u1_)),
      mw = 175, mh = 150,
      hw =  25, hh =  50,
      bg = "yellow",
      #size = sf(20)
      size = sizeS_
    )
    
    u2_ <- if (is.na(vpair[2])) "..." else format(round(vpair[2],3), nsmall = 3)
    TextBox(
      text = bquote(u[2] ~"="~ .(u2_)),
      mw = 175, mh =  50,
      hw =  25, hh =  50,
      bg = "orange",
      #size = sf(20)
      size = sizeS_
    )
  }

  ##############################################################################
  
  
  ##############################################################################
  ##  MAIN METHOD
  ## --------------------------------------------------------------------------
  ##  The equivalent of main() from a C program, to be used for visualization
  ##############################################################################
  main <- function()
  {
    # -----------------------------------------------------------------------
    # Initialization of main-global variables
    # -----------------------------------------------------------------------
    # if seed == NULL, use system-generated seed a la base::set.seed;
    # if seed == NA, use the most recent state of the generator (e.g., if
    #    user had done an explicit set.seed call prior to executing ssq) --
    #    i.e., do nothing new with respect to setting seed;
    # otherwise, set the seed with the given (integer) value of 'seed'
    # NB: simEd::set.seed version also explicitly calls base::set.seed
    if (is.null(seed) || is.numeric(seed))  simEd::set.seed(seed)
    
    # Initialize streamlines pausing functionality
    pauseData <<- SetPausePlot(
      plotDelay = plotDelay,
      prompt    = "Hit 'ENTER' to proceed, 'q' to quit, or 'h' for help/more options: "
    )

    PauseCurrPlot <- function(pauseData_, currStep) 
    {
        updatedPauseData <- PausePlot(
            pauseData = pauseData_, # list
            currStep  = currStep,  # integer
            maxSteps  = n
        )
        return(updatedPauseData)
    }

    PlotFigures <- function(v1, v2, accepted)
    {
        ResetPlot() # Clear out the plot for the next flush
        TogglePlot(c(10,190,0,190))
        if (showBoxes) DrawBorder("black")
        if (showTitle) title(titleText, line = -1, cex.main = 1.2)
    
        # Draw all of the mandatory components
        vpair <- c(v1, v2)
    
        TogglePlot(overplotRange)
        DrawBorder("grey", "white")
    
        DrawHistPlot(vpair, accepted, AccXs, AccYs)
        DrawARPlot(vpair, accepted, AccXs, AccYs, RejXs, RejYs)
        DrawVarMap(vpair, length(AccXs), length(RejXs))
    }

    ##############################################################################
    ## Printing of initial empty graphs prior to any meaningful computation
    ##############################################################################

    RejXs <- c()
    RejYs <- c()
    AccXs <- c()
    AccYs <- c()
    
    # Force layout to show only outputted plots if possible
    if (plot) 
    {
        if (is.null(dev.list())) 
            dev.new(width = 5, height = 6) 
        par(mfrow = c(1, 1), mar = c(1,1,1,1), new = FALSE)
        PlotFigures(NA, NA, FALSE)
        dev.flush(dev.hold())
        dev.hold()
    }

    while (length(AccXs) + length(RejXs) < maxTrials && 
           length(AccXs) < n) 
    {
        if (typeof(majorizingFcn) == "closure" && useDefaultMajorizingFcn)
        {
            # constant majorizing function
            #v1 <- vunif(1, 0, 1, stream = 1)    # grab a U(0,1)
            v1 <- vunif(1, support[1], support[2], stream = 1)
            v2 <- vunif(1, 0, majorizingFcn(v1), stream = 2) # gen'd y-variate
        }
        else if (typeof(majorizingFcn) == "closure" && !useDefaultMajorizingFcn)
        {
            u  <- vunif(1, 0, 1, stream = 1)    # grab a U(0,1)
            v1 <- inversionFcn(u)
            v2 <- vunif(1, 0, majorizingFcn(v1), stream = 2) # Generated y-variate
        }
        else
        {
            # data.frame majorizing function -- the appropriate inversionFcn
            # and mappingFcn are created 
            u  <- vunif(1, 0, 1, stream = 1)    # grab a U(0,1)
            v1 <- inversionFcn(u) # Generated x-variate
            v2 <- vunif(1, 0, mappingFcn(v1), stream = 2) # Generated y-variate
        }

        accepted <- (v2 <= pdf(v1))

        if (accepted) {
            AccXs <- c(AccXs, v1)
            AccYs <- c(AccYs, v2)
            pauseData$isJumpStep <- TRUE
        } else {
            RejXs <- c(RejXs, v1)
            RejYs <- c(RejYs, v2)
            pauseData$isJumpStep <- FALSE
        }

        if (plot && pauseData$plotDelay == -2 &&
                length(AccXs) == pauseData$jumpTo)
        {
            pauseData$plotDelay <- -1  # back to interactive
            pauseData$jumpComplete <- TRUE
        }
      
        if (plot)
        {
            # plot if non-zero delay (>0) or interactive (-1), 
            # but not if jumping (-2) or plot only at end (0)
            if (pauseData$plotDelay > 0 || pauseData$plotDelay == -1)
            {
                PlotFigures(v1, v2, accepted)
            }

            pauseData <- PauseCurrPlot(pauseData, length(AccXs))
            if (pauseData$menuChoice == "q")
            {
                plot <- FALSE
                break
            }
            # jumping handled w/in compPlot.R:PausePlot and
            # by jumpComplete bailout above
        }

    }  # while (...)
    
    if (plot)
    {
        PlotFigures(v1, v2, accepted)
        dev.flush(dev.hold())
        par(fig = c(0,1,0,1), mfrow = c(1,1), mar = c(5.1,4.1,4.1,2.1))
    }
      
    return(AccXs)

  } # mainvis
  ####################################################################################

  # ********************************************************************
  # * CALL THE MAIN FUNCTION, executing the simulation, return result. *
  # ********************************************************************
  # if (!plotDelay || plot) return(mainvis())
  return(main())

}
