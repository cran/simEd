#)# -------------------------------------------------------------------------
#' Lehmer Generator Visualization
#'
#' @description This function animates the processes of a basic Lehmer 
#' pseudo-random number generator (PRNG). Also known in the literature as a 
#' multiplicative linear congruential generator (MLCG), the generator 
#' is based on the formula: 
#' 
#' \deqn{X_{k+1} \equiv a \cdot X_k \pmod{m}}{%
#'       X_{k+1} = a * X_k mod m}
#'
#' where 'm' is the prime modulus, 'a' is the multiplier chosen from \{1, m-1\},
#' and 'X_0' is the initial seed chosen from \{1, m-1\}.  The random numbers 
#' generated in (0,1) are X_\{k+1\}/m.
#'
#' @param a             multiplier in MLCG equation.
#' @param m             prime modulus in MLCG equation.
#' @param seed          initial seed for the generator, i.e., the initial value X_0 
#' @param animate       should the visual output be displayed.
#' @param numSteps      number of steps to animate; default value is Inf if 
#'                      \code{plotDelay} is -1, or the size of the period
#'                      otherwise.  Ignored if animate is false.
#' @param title         optional title to display in plot (NA uses default title)
#' @param showTitle     if TRUE, display title in the main plot.
#' @param plotDelay     wait time between transitioning; -1 (default) for interactive
#'                      mode, where the user is queried for input to progress.
#'
#' @returns the entire period from the PRNG cycle, as a vector of integers in \{1, m-1\}.
#'
#' @references
#' Lehmer, D.H. (1951).  Mathematical Models in Large-Scale Computing Units.
#'    _Ann. Comput. Lab_.  Harvard University, **26**, 141-146.
#'
#' @concept  random variate generation
#' 
#' @importFrom shape plotellipse
#'
#' @examples
#'  # Default case (m, a = 31, 13); small full period
#'  lehmer(plotDelay = 0, numSteps = 16)
#'  lehmer(numSteps = 10, plotDelay = 0.1)   # auto-advance mode
#'
#'  if (interactive()) {
#'    lehmer(plotDelay = -1)  # plotDelay -1 uses interactive mode
#'  }
#'  
#'  # multiplier producing period of length 5, with different seeds
#'  lehmer(a = 8, m = 31, seed = 1, numSteps = 5, plotDelay = 0.1)
#'  lehmer(a = 8, m = 31, seed = 24, numSteps = 5, plotDelay = 0.1)
#' 
#'  # degenerate cases where seed does not appear in the final period
#'  lehmer(a = 12, m = 20, seed = 7, numSteps = 4, plotDelay = 0.1)  # length  4
#'  lehmer(a = 4, m = 6, seed = 1, numSteps = 1, plotDelay = 0.1)  # length 1
#'  
#' @export
################################################################################
lehmer <- function(
                a          = 13,
                m          = 31,
                seed       = 1,
                animate    = TRUE,
                numSteps   = NA,
                title      = NA,
                showTitle  = TRUE,
                plotDelay  = -1
                #fontScaleRatio = c(3, 1)  # save for future release
          ) 
{
  if (animate)
  {
    # using on.exit w/ par per CRAN suggestion (add 22 Nov 2023)
    oldpar <- par(no.readonly = TRUE)  # save current par settings (add 22 Nov 2023)
    on.exit(par(oldpar))               # add (22 Nov 2023)
  }

  ################################################################################
  # variables defined w/in scope of lehmer that make "good use of 
  # superassignment" for stateful function use (mod 23 Nov 2023)
  # (https://stat.ethz.ch/pipermail/r-help/2011-April/275905.html)
  # (https://adv-r.hadley.nz/function-factories.html#stateful-funs)
  #
  plottedXs <- c() # to be updated when period determined
  plottedYs <- c()
  ################################################################################

  #############################################################################
  # Do parameter checking and handling; stop execution or warn if erroneous
  #############################################################################
  checkVal(m, "i", min = 1)
  checkVal(a, "i", min = 1, max = m - 1)
  checkVal(seed, "i", min = 1, max = m - 1)
  checkVal(numSteps, "i", min = 1, na = TRUE)
  if (a >= m) 
    stop("'a' must be strictly less than 'm'")
  if (m %% a == 0) 
    stop("'a' cannot be a factor of 'm'")

  checkVal(animate,   "l")
  checkVal(title,     "c", na = TRUE, null = TRUE)
  checkVal(showTitle, "l")

  if (!isValNum(plotDelay) || (plotDelay < 0 && plotDelay != -1))
    stop("'plotDelay' must be a numeric value (in secs) >= 0 or -1 (interactive mode)")
  if (plotDelay > 10)
    message("'plotDelay' of ", plotDelay, "s may give undesirably long time between plots")
  
  #if (any(is.na(fontScaleRatio)) || length(fontScaleRatio) < 2) {
  #  stop("fontScaleRatio must be a list of two values")
  #}

  #############################################################################

  # Creating global instance of PausePlot. To be overridden in main
  PauseCurrPlot <- function() return(NA)
  
  #sf <- function(n) ScaleFont(n, f = 2, r = fontScaleRatio)   # font-scaling function
  sf <- function(n) ScaleFont(n, f = 2, r = c(3,1))   # font-scaling function

  # Construct title for the plot
  if (is.na(title) || is.null(title)) {
    titleText <- bquote(bold(
        "Multiplicative Lehmer Generator(a = "~.(a)~", m = "~.(m)~", "~x[0]~" = "~.(seed)~")"))
  } else {
    titleText <- title
  }

  #################################################################################
  ##  Define graphical components
  #################################################################################
  # Ranges for Plots (Keep bounds in (10, 190))
  # -------------   xmin, xmax, ymin, ymax ------------
  cycleRange <-    # Cycle Plot range
        if (Sys.getenv("RSTUDIO") == 1)  c(110,  190,   60,  140)
        else                             c(110,  190,   50,  170)
  equationRange <- # Equation Plot Range
        if (Sys.getenv("RSTUDIO") == 1)  c( 10,   90,   60,  140)
        else                             c( 10,   90,   50,  170)
  #################################################################################


  #################################################################################
  ### -----------------   BEGIN FUNCITON DEFINITIONS FOR MAIN   --------------  ###
  #################################################################################


  #################################################################################
  ## DrawCycle
  ## --------------------------------------------------------------------------------
  ## Initialized the Cycle plot depending on specifications
  ## @param step     The current tick of the system, with 0 being at "12:00"
  ## @param seed_idx The index of the seed (0 if period does not contain seed)
  ## @param Xs       The set of all Xs computed
  #################################################################################
  DrawCycle <- function(step, seed_idx, Xs)
  {
    seedPresent <- TRUE
    if (is.na(seed_idx)) {
        seedPresent <- FALSE
        seed_idx <- 1
    }

    cbRange <- cycleRange + c(-5, 5, 0, 5) 
        # Range displacement for background components

    ScalePlots(cycleRange)

    # Draw border around cycle region
    TogglePlot(cbRange)
    DrawBorder("grey", "white")

    # Initialize main plot
    TogglePlot(cycleRange, initPlot = FALSE, mar = c(1,1,1,1))
    plot(NA, NA, xlim = c(0,200), ylim = c(0,200), xaxt = "n", yaxt = "n",
        xlab = "", ylab = "", bty = "n", las = 1, type = "s")

    t <- length(Xs)

    c_rad  <- if (t < 11) 60 else 70    # Circle_radius
    if (Sys.getenv("RSTUDIO") == "1")
    {
        f_size <- if (t < 11) 10 else 10    # Default Font size
        t_size <- if (t < 11) 14 else 10    # Default Tick font size
    }
    else
    {
        f_size <- if (t < 11) 20 else 20    # Default Font size
        t_size <- if (t < 11) 30 else 20    # Default Tick font size
    }

    # Plot main cycle ellipse (circle)
    plotellipse(rx = c_rad, ry = c_rad, mid = c(100, 100), from = -pi, to = pi, 
        col = "lightgreen")

    # Basic function to compute x and y position at end of clock hand
    tick_xy_func <- function(index) {
      angle <- pi/2 - (2 * pi * (index - 1) / t)
      return(c(cos(angle), sin(angle)))
    }
    
    # Compute current and previous indices
    curr_idx <- ((step + seed_idx) %% t) + 1
    
    percent = (step + seed_idx - 1)/t
    mainxy = tick_xy_func(curr_idx)     ## Angle to point to current value
    seedxy = tick_xy_func(seed_idx)     ## Angle to point to seed
    
    ## Plot clock hands
    lines(
      x = c(100, 100 + 1.1 * c_rad * mainxy[1]),
      y = c(100, 100 + 1.1 * c_rad * mainxy[2])
    )

    # Basic function to help compute multiplier to scale fonts relative to content
    multf <- function(x) max(nchar(toString(x)), 2)
    
    # Interval between ticks in which a textbox is displayed
    if (length(Xs) < 50) {
      tick_interval <- ceiling(length(Xs)/20)
      ticks <- 1 + 0:(t/tick_interval - 1) * tick_interval
    } else {
      ticks <- round(seq(1, t+1, length.out = t/ceiling(t/20)))
      if (ticks[length(ticks)] > t) ticks <- ticks[1:(length(ticks)-1)]
    }
    
    # This stuff does not show if there are too many components in plot
    for (i in ticks) 
    {
      # Compute appropriate clock handle angles
      xy <- tick_xy_func(i)

      # Display clock hands
      lines(
        x = c(100 + 0.9*c_rad*xy[1], 100 + c_rad*xy[1]),
        y = c(100 + 0.9*c_rad*xy[2], 100 + c_rad*xy[2])
      )

      # Background color for textboxes for X values on clocks
      bg <- NA

      # Textboxes surrounding circle
      TextBox(
        text = Xs[i],
        mw = 100 + 1.25*c_rad*xy[1], mh = 100 + 1.2*c_rad*xy[2],
        hw = 0.2 * sf(f_size) * multf(Xs[i]), hh = 10,
        bg = bg,
        size = sf(t_size)
      )
    }
    
    # Render original seed location box
    hwmult_ <- if (Sys.getenv("RSTUDIO") == 1) 0.3 else 0.2
    if (seedPresent) {
        TextBox(
            text = Xs[seed_idx],
            mw  = 100 + 1.25*c_rad*seedxy[1], mh = 100 + 1.2*c_rad*seedxy[2],
            #hw  = 0.2 * sf(f_size) * multf(Xs[seed_idx]), hh = 10,
            hw  = hwmult_ * sf(f_size) * multf(Xs[seed_idx]), hh = 10,
            bg  = "red",
            col = "white",
            size = sf(t_size)
        )
    }
    
    # Render current location box
    TextBox(
      text = Xs[curr_idx],
      mw = 100 + 1.25*c_rad*mainxy[1], mh = 100 + 1.2*c_rad*mainxy[2],
      #hw = 0.2 * sf(f_size) * max(multf(Xs[i]), 2), hh = 10,
      hw = hwmult_ * sf(f_size) * max(multf(Xs[i]), 2), hh = 10,
      bg = "yellow",
      size = sf(t_size)
    )

    # Center text box for uniform value
    TextBox(
      text = format(round(Xs[curr_idx]/m, 3), nsmall = 3),
      mw = 100, mh = 100,
      hw = 50, hh = 20,
      bg = "cadetblue1",
      size = sf(f_size*2)
    )
    
    cexmain_ <- if (Sys.getenv("RSTUDIO") == 1) 1.2 else 1.4
    title(paste(sep="", "Generation Circle (Period = ", t, ")"), cex.main = cexmain_)
  }
  ##################################################################################

  ##################################################################################
  ## DrawEquation
  ## ------------------------------------------------------------------------------
  ## Draw the relationship equations based on current state
  ## @param x_idx   The index of the previous x (0-base)
  ## @param xc      The previous x that was generated
  ## @param xn      The latest x to be generated
  ##################################################################################
  DrawEquation <- function (x_idx, xc, xn)
  {
    ebRange  <- equationRange + c(-5, 5, 0, 5)

    ScalePlots(equationRange)
    
    # Toggle to subplot and draw border 
    TogglePlot(ebRange)
    DrawBorder("grey", "white")

    # Toggle to main plot and initiate coordinate-bound plot
    TogglePlot(equationRange, initPlot = FALSE, mar = c(1,1,1,1))
    plot(NA, NA, xlim = c(0,200), ylim = c(0,200), xaxt = "n", yaxt = "n",
         xlab = "", ylab = "", bty = "n", las = 1, type = "s")
    # Special plot call to use 's' option

    size_ = if (Sys.getenv("RSTUDIO") == 1) sf(16) else sf(30)
    TextBox( # Plots basic semi-static recurrence relation: X_{n} = a * X_{n-1} mod m
      text = bquote(X[.(x_idx+1)] == a %.% X[.(x_idx)] ~ "mod" ~ m),
      # text = Encoding(bquote(X[.(x_idx+1)] == a %.% X[.(x_idx)] ~ "mod" ~ m)),
      mw = 100, mh = 170,
      hw = 100, hh = 30,
      size = size_
    )

    tbmh <- 120     # Mid height for textbox (for plug-in equation)
    tbhh <-  15     # Half height for textbox elements
    #tbsz <- sf(20)  # Textbox font size for textbox elements
    tbsz <- if (Sys.getenv("RSTUDIO") == 1) sf(12) else sf(20)
    
    { # Plots basic non-static recurrence relation: X_{n} = a * X_{n-1} mod m
      texts <- c(xn, paste("=", a, "\U2022"), xc, paste("mod", m))
      TextBox(text = texts[1], mw =  12, mh = tbmh, hw = 16, hh = tbhh, 
              size = tbsz, bg = "yellow")
      TextBox(text = texts[2], mw =  61, mh = tbmh, hw = 29, hh = tbhh, 
              size = tbsz)
      TextBox(text = texts[3], mw = 107, mh = tbmh, hw = 17, hh = tbhh, 
              size = tbsz, bg = "lightgrey")
      TextBox(text = texts[4], mw = 162, mh = tbmh, hw = 38, hh = tbhh, 
              size = tbsz)
    }

    segments(x0 = 20, y0 = 85, x1 = 180) # Breakage line

    tbmh <- 50     # Mid height for textbox (for plug-in equation)
    tbhh <- 15     # Half height for textbox elements
    #tbsz <- sf(20) # Textbox font size for textbox elements
    
    { # Plots basic normalization equation: X_{n} / m = u
      segments(x0 = 65, y0 = 40, x1 = 75, y1 = 60)
      # segments(x0 = 20, y0 = 40, x1 = 180) # Breakage line
      texts <- c(xn, paste(m, " = ", sep = ""), 
                 format(round(xn/m, 3), nsmall = 3))
      TextBox(text = texts[1], mw =  35, mh = tbmh, hw = 16, hh = tbhh,
              size = tbsz, bg = "yellow")
      TextBox(text = texts[2], mw =  95, mh = tbmh, hw = 29, hh = tbhh, 
              size = tbsz)
      TextBox(text = texts[3], mw = 155, mh = tbmh, hw = 24, hh = tbhh,
              size = tbsz, bg = "cadetblue1")
    }

  }

  ####################################################################################

  ####################################################################################
  ##  DrawLinePlot (and associated function-scope holders)
  ## --------------------------------------------------------------------------------
  ##  Initialized the generated value plot depending on specifications
  ##  @param xn      Point just generated; flashes as light blue
  ##  @param period  How long should the generated list be
  ####################################################################################
  #plottedXs <- c() # to be updated when period determined  (del 23 Nov 2023)
  #plottedYs <- c() #                                       (del 23 Nov 2023)

  DrawLinePlot <- function(xn, seed, seedPresent = TRUE)
  {
    lineplotRange <- c(10, 190, 20, 50)        
    lpRange  <- lineplotRange + c(-5, 5, 0, 5)
        # Coordinate system mutation for background

    # Scale and toggle to the plot range and proceed to plot as needed
    ScalePlots(lineplotRange)
    TogglePlot(lineplotRange, initPlot = FALSE, mar = c(1,1,1,1))
    plot(NA, NA, xlim = c(0,1), ylim = c(-1,1), yaxt = "n",
        xaxp = c(0,1,10), cex.axis = 1.4,  # bgl
        xlab = "", ylab = "", bty = "n", las = 1, type = "s")

    cex <- if (length(plottedXs) >= 100) 1.5 else 2
      
    # Draw all of the points as necessary -- remember seed sits @ front
    where <- which(plottedXs == xn)
    points(x = plottedXs[1:where]/m, y = plottedYs[1:where], bg = "grey", 
        pch = 21, cex = cex)
    ptColor <- "cadetblue1"
    if (seedPresent && xn == seed) ptColor <- "red"
    points(x = xn/m, y = 0, bg = ptColor, col = "black", pch = 21, cex = 2)
  }

  ##################################################################################


  ##################################################################################
  ##  MAIN METHOD
  ## --------------------------------------------------------------------------------
  ##  The equivalent of main() from a C program.  This function will be
  ##  invoked at the end of the encompassing ssq() function, causing the
  ##  discrete-event simulation to execute.
  ##################################################################################
  main <- function(seed)
  {
    if (animate) 
    {
        if(is.null(dev.list())) 
            dev.new(width = 7, height = 4.5)
        par(mfrow = c(1, 1), mar = c(1,1,1,1), new = FALSE)
        dev.flush(dev.hold())
        dev.hold()
    }

    ##############################################################################
    ## Printing of initial empty graphs prior to any meaningful computation
    ##############################################################################

    i   <- 1                 # Appending index
    xn  <- 0                 # New X
    xc  <- seed              # Current X
    Xs  <- rep(-1, m-1)      # Set of all possible X's, starting at seed
    Xs[i] <- seed

    # Find Period
    while (i <= m) 
    {
      i  <- i + 1
      xn <- (a * xc) %% m
      
      # If the new x value is the same as the seed, the cycle is done
      # If we have a repeating element, we have degenerated
      if (xn == seed || xn == xc || xn %in% Xs) break 
      
      # Transfer new x into X set and set it to be the current x
      Xs[i] <- xn
      xc <- xn
    }

    # remove the -1's
    Xs <- Xs[which(Xs != -1)]
    
    # If the current X is equal to the previous X, the period has degenerated to 0
    # Otherwise, record period as normal
    if (xn == xc) {             # Edge case, period of 1
      period <- 1
      Xs <- c(xn)
      Xs_reorg <- Xs
      seed_idx <- if (xn == seed) 1 else NA
    } 
    else 
    {
      # note that in some unruly cases (e.g., a = 8, m = 30, x0 = 7), a
      # non-full-period will result that does not include the initial seed -- 
      # the code below should handle well-behaved and unruly cases
      startIdx <- which(Xs == xn)[1]
      endIdx <- length(Xs)
      Xs <- Xs[startIdx:endIdx]
      period <- endIdx - startIdx + 1

      # presuming 1 is in the period, reorganize so that 1 will be drawn
      # at the top of the clock
      if (1 %in% Xs && Xs[1] != 1) {
        where <- which(Xs == 1)
        Xs_reorg <- c(Xs[where:length(Xs)], Xs[1:(where - 1)])
      } else {
        Xs_reorg <- Xs
      }

      # find where the seed lives, if at all
      if (seed %in% Xs_reorg) {
        seed_idx <- which(Xs_reorg == seed)[1]
      } else {
        seed_idx <- NA
      }
    }

    message("Period found to be ", period)
    if (!seed %in% Xs_reorg) {
        message("Warning: initial seed not in resulting period")
    }

    # set up variables used in drawing line plot at bottom of window; for jumping,
    # computationally expensive to pass these with every function call; also, 
    # swap the seed (first element if seed not present) to the end for this
    # since we want to plot it last
    if (length(Xs) >= 2) {
        plottedXs <<- c( Xs[2:length(Xs)], Xs[1] )
        plottedYs <<- rep(0, length(plottedXs))
    } else {
        # add 23 Nov 2023: additional logic to handle 1-length degenerate case
        plottedXs <<- c( Xs[1] )
        plottedYs <<- rep(0, length(plottedXs))
    }

    if (is.na(numSteps))
        numSteps <- if (animate && plotDelay == -1) Inf else period

    pauseData <- SetPausePlot(
      plotDelay = plotDelay, 
      prompt    = "Hit 'ENTER' to proceed, 'q' to quit, or 'h' for help/more options: "
    )

    PauseCurrPlot <- function(pauseData, currStep) 
    {
        updatedPauseData <- PausePlot(
            pauseData      = pauseData,     # list
            currStep       = currStep,      # integer
            maxSteps       = numSteps       # integer
        )
        return(updatedPauseData)
    }

    DrawComponents <- function(i, xc, xn)
    {
        ResetPlot() # Clear out the plot for the next flush
        if (showTitle)
            if (Sys.getenv("RSTUDIO") == 1)
                title(titleText, line = -1.5, cex.main = 1.0)
            else
                title(titleText, line = -0.6, cex.main = 1.2)
        DrawCycle(i - 1, seed_idx, Xs_reorg)
        DrawEquation(i - 1, xc, xn) 
        DrawLinePlot(xn, seed, seedPresent = !is.na(seed_idx))
    }
    
    i <- 1  # Current step
    xc <- Xs[((i - 1) %% length(Xs)) + 1]
    xn <- Xs[((i + 0) %% length(Xs)) + 1]

    while (animate && pauseData$plotDelay != 0 && i < numSteps) 
    {
        # plot if non-zero delay (>0) or interactive (-1), 
        # but not if jumping (-2) or plot only at end (0)
        if (pauseData$plotDelay > 0 || pauseData$plotDelay == -1)
        {
            DrawComponents(i, xc, xn)
        }

        pauseData <- PauseCurrPlot(pauseData, i)

        if (pauseData$menuChoice == "q")   # quit immediately
        { 
            numSteps <- i
            break
        }
        else if (pauseData$menuChoice == "e") 
        {
            # pauseData: "e":progress to end w/o plotting until end
            # change numSteps to next larger multiple of period
            p <- period
            # user may have specified a number of steps even for interactive
            if (is.infinite(numSteps)) {
                numSteps <- (p * floor(i / p)) + (p * ceiling((i / p) %% 1))
            }
            i <- numSteps
            break
        } 
        else if (pauseData$menuChoice == "j")
        {
            # pauseData: "j":jump ahead w/o plotting until jump stop;
            # NOTE: because for lehmer the period is already generated and
            #   stored in Xs[], we don't need to navigate the while loop
            #   that those plots can be saved -- so, just advance i and
            #   switch back to interactive
            i <- pauseData$jumpTo
            pauseData$plotDelay <- -1 # back to interactive
            pauseData$jumpComplete <- TRUE
        }
        else  # regular step advance
        {
            i <- i + 1    
        }
        xc <- Xs[((i - 1) %% length(Xs)) + 1]
        xn <- Xs[((i + 0) %% length(Xs)) + 1]
    }

    if (animate) 
    {
      if (plotDelay == 0) 
      {
        # this final plot is the only plot
        i <- numSteps
        xc <- Xs[((i - 1) %% length(Xs)) + 1]
        xn <- Xs[((i + 0) %% length(Xs)) + 1]
      } 
      else if (plotDelay == -1 && pauseData$menuChoice == "e") 
      {
        xc <- Xs[((i - 1) %% length(Xs)) + 1]
        xn <- Xs[((i + 0) %% length(Xs)) + 1]
      }
      DrawComponents(i, xc, xn)

      # reset to defaults the par values Vadim's plotting code overrides
      dev.flush(dev.hold())
      par(fig = c(0,1,0,1), mfrow = c(1,1), mar = c(5.1,4.1,4.1,2.1))
    }

    # put the initial seed at the end, since it is not the first number
    # generated in the sequence
    if (seed == Xs[1]) Xs <- c(Xs[2:length(Xs)], Xs[1])

    # if fewer steps than numbers, return only those generated
    if (numSteps < length(Xs)) return(Xs[1:numSteps])
    # if more steps than numbers, return cycles generated
    if (numSteps %% length(Xs) > 0) {
        return(c(rep(Xs, floor(numSteps / length(Xs))),
                 Xs[1:(numSteps %% length(Xs))]))
                 # Xs[1:0] unfortunately returns Xs[1]
    }
    # this will handle any multiple of numSteps
    return(rep(Xs, numSteps / length(Xs)))

  } # main
  ####################################################################################

  # ********************************************************************
  # * CALL THE MAIN FUNCTION, executing the simulation, return result. *
  # ********************************************************************
  return(main(seed))

}
