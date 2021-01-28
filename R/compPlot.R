###############################################################################
##  NewPlot
## ----------------------------------------------------------------------------
##  Makes new blank plot of specifiable range (default from 0 to 200)
##  Comes preloaded with defaults optimized for non-graph plots;
##  Recommended to use own plot function when plotting graphs
###############################################################################
NewPlot <- function(xlim = c(0, 200), ylim = c(0, 200)) {
    plot(NA, NA, xlim = xlim, ylim = xlim, xaxt = "n", yaxt = "n",
        xlab = "", ylab = "", bty = "n", las = 1)
}
###############################################################################


###############################################################################
##  DrawBorder
## ----------------------------------------------------------------------------
##  Draws a rectangular border or rectangle of specified color that extends
##  across full range of default PlotCoordRange
###############################################################################
DrawBorder <- function(br, col = rgb(0,0,0,0),
                       xLowerLeft = 0, yLowerLeft = 0, 
                       xUpperRight = 200, yUpperRight = 200) {
  rect(xLowerLeft, yLowerLeft, xUpperRight, yUpperRight, 
       border = br, col = col, lwd = 2)
  #rect(0, 0, 200, 200, border = br, col = col, lwd = 2)
}
###############################################################################


###############################################################################
##  ResetPlot
## ----------------------------------------------------------------------------
##  Pad the current plot components to fill up the graph;
##  This keeps everything in animation consistently located
###############################################################################
ResetPlot <- function() {
  # Initialize a new plotting window overlaying the current one
  par(mfrow = c(1,1), new = FALSE)
  # dev.off(); dev.new()          # Alternative 
  NewPlot()
}
###############################################################################


###############################################################################
##  ScalePlot
## ----------------------------------------------------------------------------
##  Scales initial plot to a set dimension and specifies plot boxes to scale to
###############################################################################
ScalePlots <- function(dims, mfrow = c(2, 3)) {
  # Setting mfrow here scales all subplots as if to fit 3,2 plot
  par(fig = dims/200, mfrow = mfrow, mar = c(0,0,0,0), new = TRUE)
  NewPlot()
}
###############################################################################


###############################################################################
##  TogglePlot
## ----------------------------------------------------------------------------
##  Toggle to a new plot with new dimension specification
###############################################################################
TogglePlot <- function(dims, initPlot = TRUE, mar = c(0,0,0,0)) {
  par(fig = dims/200, mar = mar, new = TRUE)
  if (initPlot)  NewPlot()
}
###############################################################################



###############################################################################
# ScaleFont  -  Font-Scaling Function
# -----------------------------------------------------------------------------
# Font-Scaling Function
#
# @description Returns a scaling of the inputted number based on the device.
#
# @param n Size to scale to
#
# @return A numeric value of the scaled n
#
# @template signature
# @keywords internal
###############################################################################
ScaleFont <- function(n, f = 100, r = c(1,1)) {
    return(n/f * min(dev.size() / r))
}
###############################################################################


###############################################################################
# TextBox  -  Plot Text Box
# -----------------------------------------------------------------------------
# Plot Text Box
#
# @description This function plots a text box with a variety of options,
#    including a scaled raster image for the background.
#
# @param text    text to be displayed in the box.
# @param mw      mid-width of the text box
# @param mh      mid-height of the text box
# @param hw      half-width of the text box
# @param hh      half-height of the text box
# @param col     color of text
# @param bg      color of background box (NA for no background)
# @param img     raster image to be displayed as background
# @param font    font style of text
#                (1 = plain, 2 = bold, 3 = italic, 4 = bold-italic)
# @param size    scaled font-size of text
# @param txd     text x-displacement from center as a percentage of half-width
# @param tyd     text y-displacement from center as a percentage of half-height
# @param textf   function to be applied to text prior to displaying
# @param xs      scaling function to be applied to x
# @param ys      scaling function to be applied to y
#
# @template signature
# @keywords internal
###############################################################################
TextBox <- function(
  text = "", mw, mh, hw = NA, hh = hw,
  col = "black", bg = NA, border = "black", img = NA,
  font = 1, size = 15, txd = 0, tyd = 0,
  textf = 0, xs = 0, ys = 0
) {
  if (!is.function(xs))    xs    <- function(c) return(c)
  if (!is.function(ys))    ys    <- function(c) return(c)
  if (!is.function(textf)) textf <- function(t) return(t)

  tryCatch({
    if ((is.numeric(text) || is.character(text)) && is.na(text))  text <- "-"
  }, error = function(e) {
    stop(paste("Error in TextBox: invalid text '", text, "' of type", typeof(text)))
  })
  if (text == "i")  font <- 3  # To be phased out later
  size <- ScaleFont(size)

  # Shift midwidth and midheight to not go over edge
  mw <- max(min(mw, 200 - hw), hw)
  mh <- max(min(mh, 200 - hh), hh)
  hw1 <- hw
  hh1 <- hh

  if (is.na(img[1])) {

    if (!is.na(bg)) {
      rect(xs(mw - hw), ys(mh - hh), xs(mw + hw), ys(mh + hh), 
            col = bg, border = border)
    }

  } else {
    
    if (!requireNamespace("magick", quietly = TRUE))
      stop("Package \"magick\" needed for images to work. Please install.", 
           call. = FALSE)

    picratio <- (dim(img)[1] / dim(img)[2])
    pltratio <- 1/diff(par("fig")[3:4])
    yratio   <- pltratio * picratio
    hhnew    <- hw * yratio

    if (hhnew > hh)
      hw1 <- hw  * hh1/hhnew
    else
      hh1 <- hhnew

    rasterImage(img,
      xs(mw - hw1), ys(mh - hh1),
      xs(mw + hw1), ys(mh + hh1))
  }

  text(
    xs(mw + hw * txd), ys(mh + hh * tyd),
    labels = textf(text), col = col, cex = size, font = font)
}
################################################################################



################################################################################
# DrawPoint  -  Plot Point with fill and outline
# -----------------------------------------------------------------------------
# Plot Text Box
#
# @description This function plots a Point with outline
#
# @param x    X-coordinate
# @param y    Y-coordinate
# @param col     Color of point fill
# @param cex     Size of the point
# @param bg      Outline/Background of point
#
# @template signature
# @keywords internal
################################################################################
DrawPoint <- function(x, y, col, cex = 1.5, bg = "black")
{
  lx <- length(x)
  ly <- length(y)

  if (lx * ly > 1) {
    if (lx != ly && min(lx, ly) > 1)
      warning(paste("DrawPoint() called with |x| ==", lx, "and |y| ==", ly))
    else for (i in 1:max(lx, ly))
      DrawPoint(
        x   = x[min(lx, i)],
        y   = y[min(ly, i)],
        col = col,
        cex = cex,
        bg  = bg
      )
    return()
  }

  isInvalid <- suppressWarnings(is.na(as.numeric(x)) || is.na(as.numeric(y)))
  if (isInvalid)  return()

  #points(x, y, pch = 20, cex = cex, col = col, xpd = NA)
  #points(x, y, pch = 21, cex = cex, col = bg, xpd = NA)
  points(x, y, pch = 20, cex = cex, col = col)
  points(x, y, pch = 21, cex = cex, col = bg)
}
################################################################################



################################################################################
# SetPausePlot  -  Pause Plot Value Setter
# -----------------------------------------------------------------------------
# Pause Plot Value Setter
#
# @description Overrides currently set/default value of \code{PausePlot} calls.
#
# @details See \code{PausePlot} for more thorough description of parameters
#
# @param prompt       Default prompt for \code{PausePlot} calls
# @param viewCommand  Default view command for \code{PausePlot} calls
# @param viewNumArgs  Number of arguments required by associated viewCommand
# @param viewInstruct Default view instruction for \code{PausePlot} calls
# @param viewFunction Default view function for \code{PausePlot} calls
#
# @template signature
# @keywords internal
################################################################################
SetPausePlot <- function(plotDelay      = -1,
                         prompt         = NA,
                         viewCommand    = NULL,
                         viewNumArgs    = NULL,
#^(1)
                         viewInstruct   = NULL,
                         viewFunction   = NULL
                        )
{
  if (1 != length(unique(c(length(viewCommand), 
                           length(viewNumArgs),
#^(1)
                           length(viewInstruct), 
                           length(viewFunction)))))
  {
    warning(paste("view parameters (viewCommand, viewNumArgs, viewInstruct, and",
      "viewFunction) are not of the same length. Each element of one should",
      "correspond to the same-indexed element of the others"))
#^(1)
  }

  return(list(
    plotDelay        = plotDelay,    # 0:goto end; -1:interactive; -2:jump to job
    prompt           = prompt,
    menuChoice       = "",
    viewCommand      = viewCommand,
    viewNumArgs      = viewNumArgs,
#^(1)
    viewInstruct     = viewInstruct,
    viewFunction     = viewFunction,
    jumpTo           = NA,    # allow calling fcn to know when to reset jumpComplete
    numJumpSteps     = NA,    # used to display progress bar when jumping
    stepsUntilFlush  = NA,    # used to count down for jumping progress bar
    jumpComplete     = FALSE, # if true, finished with jump -> start plotting again
    isJumpStep       = TRUE,  # calling fcn can determine when a "jump step" has
                              #   has occurred based on its context (e.g., next
                              #   num for lehmer; next job to arrive for ssqvis)
    endOfSim         = FALSE, # used to avoid query @ end of simulation
    progressBar      = utils::txtProgressBar(min = 0, max = 1, char = '.',
                                             initial = -1, style = 3)
    #drawThisPlot     = TRUE   # whether to draw the curr plot (for efficiency,
                              #    this can be set to FALSE during jumps)
  ))
}
################################################################################



################################################################################
# PausePlot  -  Pause Plot For User Input
# -----------------------------------------------------------------------------
# Pause Plot For User Input
#
# @description
#    Handles pausing of plot at regular intervals depending on plotDelay.
#    Also handles user input and processing for plotDelay == -1
#
# @param plotDelay    Current delay setting, in secs. If -1, prompt for input
# @param prompt       Default prompt for \code{PausePlot} calls
# @param currStep     A numerical value representing a 'current step' count.
#                       Specifying this will enable jump command. Should be 
#                       positive
#
# @return A potentially-updated pauseData
# 
# @importFrom grDevices dev.flush dev.hold recordPlot replayPlot
#
# @template signature
# @keywords internal
################################################################################

########################################
displayInteractiveMenu <- function(pauseData,
                                   currStep,
                                   maxSteps
                                  )
{
    input <- "_"  # bogus initial input

    # While the input is not just an enter, keep looping request for input
    while (pauseData$plotDelay == -1 && input != "") 
    {
        # message("Interactive mode, waiting for input")
        input <- readline(pauseData$prompt)
        if (input == "n" || input == "next") input <- "" # same as enter

        if (input != "") 
        {
            # split input into vector of separate words, e.g., equivalent of
            #       c("h")         or          c("jump","39")
            input_<- tolower(unlist(strsplit(input, split = " ", fixed = TRUE)))

            if (input_[1] == "q" || input_[1] == "quit") 
            {
                # Handle quitting immedately
                pauseData$plotDelay <- 0
                pauseData$menuChoice <- "q"
                return(pauseData)
            }
            else if (input_[1] == "e" || input_[1] == "end") 
            {
                # Handle completion to end
                pauseData$plotDelay <- 0
                pauseData$menuChoice <- "e"
                return(pauseData)
            }
            else if (input_[1] == "j" || input_[1] == "jump")
            {
                pauseData$menuChoice <- "j"

                # Handle jump function (skips until job number reached)
                arg <- as.numeric(gsub('[a-zA-Z *]','', input))

                if (is.na(arg))
                    message("\t", sym$alert, 
                        "  'jump' needs desired step number (e.g., 'jump 5')")
                else if (arg <= currStep) {
                    message("\t", sym$alert, 
                        " 'jump' needs a step greater than ", currStep)
                } else {
                    # push all the work of changing from plotDelay == -2 to == -1 
                    # onto the calling code (ssq or ssqvis or PlotContinuous or ...)
                    # since they can more easily handle each different situation
                    pauseData$plotDelay       <- -2
                    pauseData$jumpComplete    <- FALSE
                    pauseData$isJumpStep      <- FALSE
                    pauseData$jumpTo          <- arg
                    pauseData$numJumpSteps    <- arg - currStep
                    pauseData$stepsUntilFlush <- pauseData$numJumpSteps
                    pauseData$progressBar     <- 
                        utils::txtProgressBar(min = 0, max = 1, char = '.',
                                              initial = -1, style = 3)
                    return(pauseData)
                }
            }
            else if (input_[1] == "h" || input_[1] == "help" || input_[1] == "?")
            {
                pauseData$menuChoice <- "h"

                message("\t'n'/'next'/ENTER  = proceed to next step")
                if (!is.na(currStep))
                    message("\t'j'/'jump'        = jump to the nth step")

                # Handle help function -- unfortunately, 1:0 in R returns
                # the vector c(1,0)...
                if (length(pauseData$viewCommand) > 0) {
                    for (m in 1:length(pauseData$viewCommand)) {
                        msg <- paste("\t", pauseData$viewInstruct, sep = "")
                        message(msg)
                    }
                }
          
                message("\t'e'/'end'         = proceed immediately to ending plot")
                message("\t'q'/'quit'        = quit immediately")
            }
            else 
            {
                pauseData$menuChoice <- pauseData$viewCommand

                # Shows job statistics if requested
                for (opt in 1:length(pauseData$viewCommand)) 
                {
                    tryCatch(
                        if (!is.na(input) &&
                            nchar(input) >= nchar(pauseData$viewCommand[opt]) &&
                            substr(input, 1, nchar(pauseData$viewCommand[opt]))
                                == pauseData$viewCommand[opt])
                        {
                            #arg <- as.numeric(gsub('[a-zA-Z *]','', input))
                            #if (is.na(arg)) arg <- currStep
                            if (pauseData$viewNumArgs[opt] == 1) {
                                arg <- as.numeric(gsub('[a-zA-Z *]','', input))
                            } else if (pauseData$viewNumArgs[opt] == 0) {
                                arg <- currStep
                            } else {
                            }
#^(9)
                            pauseData$plotDelay <- 
                                pauseData$viewFunction[[opt]](arg)
                            if (is.null(pauseData$plotDelay) || 
                                is.na(pauseData$plotDelay))
                                pauseData$plotDelay <- -1
                        },
                        error = function(cond) {}
                    )
                }
            }
        }
        else   # input_ == "" or "n" or "next"
        {
            pauseData$menuChoice <- "n"
        }

    }  # while (pauseData$plotDelay == -1 && input != "")

    stopifnot(input == "")
    return(pauseData)
}

########################################
PausePlot <- function(pauseData,
                      prompt           = NA,
                      currStep         = 0,
                      maxSteps         = Inf,
                      closeProgressBar = FALSE
                     )
{
    if (closeProgressBar) {
        utils::setTxtProgressBar(pauseData$progressBar, 1.0)
        close(pauseData$progressBar) 
        return()
    }

    # plotDelay options:
    #    0: not interactive, not time-delayed -- jump to end plot
    #  > 0: not interactive, but time-delayed -- non-zero delay b/w each plot
    #   -1: interactive -- query for input between each plot
    #   -2: mid-jump during interactive -- will revert back to -1, but need to 
    #       to jump to end-of-jump plot, saving back plots as necessary
    if (pauseData$plotDelay == 0) 
    {
        #pauseData$drawThisPlot <- FALSE   # default
        if (currStep == 0 || currStep >= maxSteps - 1)
        {
            grDevices::dev.flush(grDevices::dev.hold())  # full flush: hold == 0
            grDevices::dev.hold()  # full flush: hold == 0
            # hack that seems to fix occassionally not displaying all of plot
            # on first (held) step
            if (currStep == 0) Sys.sleep(0.1)
            #pauseData$drawThisPlot <- TRUE  # draw plot only at start and end
        }

        stopifnot(maxSteps < Inf)
        if (pauseData$menuChoice != 'q') {
            utils::setTxtProgressBar(pauseData$progressBar, currStep / maxSteps)
            utils::flush.console()

            if (currStep >= maxSteps && !is.null(pauseData$progressBar)) {
                utils::setTxtProgressBar(pauseData$progressBar, 1.0)
                close(pauseData$progressBar) 
            }
        }

        return(pauseData)
    }
    else if (pauseData$plotDelay > 0)
    {
        dev.flush(dev.hold())  # full-flush: sets hold level to 0
        dev.hold()             # sets hold level to 1
        # sleep for appropriate amount of time, then flush and return
        Sys.sleep(pauseData$plotDelay)
        return(pauseData)
    }
    else if (pauseData$plotDelay == -1)
    {
        if (pauseData$jumpComplete) 
        {
            # just back into plotDelay == -1 from completing a jump (== -2);
            # the calling function (e.g., ssq) will let us know when it's time 
            # to stop jumping by setting pauseData$jumpComplete to TRUE; but
            # we're going to ask that function to set plotDelay back to -1
            # so that it can handle == -2 with jumpComplete
            pauseData$isJumpStep      <- FALSE
            pauseData$jumpTo          <- NA
            pauseData$numJumpSteps    <- NA
            pauseData$stepsUntilFlush <- NA
            utils::setTxtProgressBar(pauseData$progressBar, 1.0)
            cat("\n")  # don't want to close the bar, but need a newline
            utils::flush.console()
            pauseData$jumpComplete <- FALSE
            pauseData$menuChoice <- ":)"  # useless emoticon fun
        } 

        dev.flush(dev.hold())  # full-flush: sets hold level to 0
        dev.hold()             # sets hold level to 1
        if (!pauseData$endOfSim)
            pauseData <- displayInteractiveMenu(pauseData, currStep, maxSteps)
            # NOTE: we may have changed from plotDelay == -1 to
            #      - plotDelay == -2, if jumping
            #      - plotDelay ==  0, if advancing to end

        return(pauseData)

    }
    else if (pauseData$plotDelay == -2)
    {
        if (pauseData$isJumpStep)
        {
            # the calling function will let us know when it's time to advance
            # the progress bar; update the progress bar during jump
            pauseData$stepsUntilFlush <- pauseData$stepsUntilFlush - 1
            barValue <- (pauseData$numJumpSteps - pauseData$stepsUntilFlush) /
                         pauseData$numJumpSteps
            pauseData$isJumpStep <- FALSE
            utils::setTxtProgressBar(pauseData$progressBar, barValue)
            utils::flush.console()
        }
        return(pauseData)
    }

    stopifnot(FALSE)
    return(pauseData)
}



################################################################################
# PlotTimer  -  Time execution and plot results
# -----------------------------------------------------------------------------
# Times Execution and Plots Results
#
# @description
#    Helper function to help visualize component timing. Maintains running 
#    times list which is returned and should be passed in. Also serves 
#    as a \code{TogglePlot} use-case. 
#
# @param times        times component. Generated on initial \code{PlotTimer} 
#                     call.  Last element is the most recent time record or 
#                     change in time recorded; all previous entries are previous
#                     changes in time, in order of recording time. 
# @param plotRange    coordinate range of output. Consistent with
#                     \code{TogglePlot} specs:
#                     4-element vector with x0, x1, y0, and y1, in that order. 
# @param borderDisp   Displacement component used for moving background border. 
#                     4-element vector with x0, x1, y0, and y1, in that order. 
# @param start        Whether or not the timer should start with this call. 
#                     If yes, last element will be set to System time (last 
#                     recorded time). If no, last element will be set to 0 
#                     (latest change in time)
#
# @return Updated vector that should be inputted into subsequent 
#    \code{PlotTimer}/\code{ToggleTimer} calls.
#    Last element is either last-recorded time or change in time. Prior elements
#    are the changes in time recorded thus far.
#
# @template signature
# @keywords internal
################################################################################
PlotTimer <- function(
  times = c(), 
  plotRange = c(160, 200, 0, 30), 
  borderDisp = c(-15, 0, 0, 0), 
  start = TRUE, 
  print = FALSE, 
  showPlot = FALSE
){

  currtime <- Sys.time()

  if (!length(times)) {
    if (start) return(as.double(currtime))
    else       return(c(0))
  }

  # If the time is an actual time count, toggle tail element to be a change in time. 
  if (times[length(times)] > 1000000) {
    times <- ToggleTimer(times)
  }

  if (showPlot) {
    
    op <- par(no.readonly = TRUE)

    timerPlotBorderRange <- plotRange + borderDisp
    
    # Scale and toggle to the plot range and proceed to plot as needed
    ScalePlots(plotRange)
    TogglePlot(timerPlotBorderRange)
    DrawBorder("grey", "white")
    TogglePlot(plotRange, initPlot = FALSE, mar = c(1,1,1,1))
  
    plot(times, bty = "n", las = 1, type = "s")
    par(op)
    
  }

  if (print) cat("\r", "Time = ", times[length(times)])

  # Either set last element to current time (start timer) or 0 (record stopped timer)
  times <- c(times, if(start) Sys.time() else 0)
  
  return(times)
}


################################################################################
# ToggleTimer  -  Pauses or Resumes Timer
# -----------------------------------------------------------------------------
# Toggles Timer State To Either Stop or Start Recording
#
# @description
#    Helper function to help visualize component timing. Toggles tail element of 
#    \code{PlotTimer} output vector between change in time and actual time. In 
#    effect, tells timer to pause or resume timing. 
#
# @param times        times component. Generated on initial \code{PlotTimer} 
#                     call.  Last element is the most recent time record or
#                     change in time recorded; all previous entries are
#                     previous changes in time, in order of recording time. 
#
# @return Updated vector with updated state
#
# @template signature
# @keywords internal
################################################################################
ToggleTimer <- function(times = c()){

  times[length(times)] <- as.double(Sys.time() - times[length(times)])

  return(times)
}
