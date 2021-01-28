################################################################################
#  ifd - IDF Visualization Function for F Distribution
# ------------------------------------------------------------------------------
#  The ifd R function visualizes the F idf evaluated at a provided
#  uniform(0,1) u. This will graph the idf in action (via dashed lines back across
#  the cdf). Note that the u argument can be a scalar or vector.  If a vector,
#  multiple dashed lines will be displayed, and the return type will be a vector.
#  The function also gives the option of displaying a histogram of the variates
#  generated, with theoretical F superimposed.
# ------------------------------------------------------------------------------
#' @templateVar distro   F
#' @templateVar distrolc F
#' @templateVar ifunct   ifd
#' @templateVar funct    f
#' @templateVar PXF      PDF
#' @templateVar massDen  density
#' @templateVar arglong  df1 = 1, df2 = 2, ncp = 10
#' @templateVar argshort 5, 5
#' @templateVar minPQ    0.01
#' @templateVar maxPQ    0.99
#'
#' @template i-cont
#' @template -fd
#' @template i-2
#' @export
################################################################################
ifd <- function (u = runif(1), df1, df2, ncp = 0,
                minPlotQuantile = 0.01,
                maxPlotQuantile = 0.99,
                plot            = TRUE,
                showCDF         = TRUE,
                showPDF         = TRUE, 
                showECDF        = TRUE, 
                show            = NULL,
                maxInvPlotted   = 50,
                plotDelay       = 0,
                sampleColor     = "red3",
                populationColor = "grey",
                showTitle       = TRUE,
                respectLayout   = FALSE, ...)
{
  #############################################################################

  if(is.null(dev.list()))  dev.new(width=5, height=6)
  
  warnVal <- options("warn")          # save current warning setting...
  oldpar  <- par(no.readonly = TRUE)  # save current par settings

  #############################################################################

  options(warn = -1)          # suppress warnings

  if (!is.null(u) && (min(u) <= 0 || max(u) >= 1))  stop("must have 0 < u < 1")
  if (length(u) == 0)  u <- NULL

  checkVal(df1, minex = 0, define = "degree of freedom 1")
  checkVal(df2, minex = 0, define = "degree of freedom 2")
  if (!missing(ncp))
    checkVal(ncp, min = 0, define = "non-centrality param")

  checkQuants(minPlotQuantile, maxPlotQuantile, minex = 0, maxex = 1)

  options(warn = 1)                   # set to immediate warnings

  # Check for deprecated parameters
  for (arg in names(list(...))) {
    if (arg == "maxPlotTime")
      warning("'maxPlotTime' has been deprecated as of simEd v2.0.0")
    else stop(paste("Unknown argument '", arg, "'", sep = ""))
  }

  #############################################################################

  no.ncp <- missing(ncp)

  # Define getter functions
  getDensity  <- function(d)
        if (no.ncp)  df(d, df1, df2)  else  df(d, df1, df2, ncp)  #d
  getDistro   <- function(d)
        if (no.ncp)  pf(d, df1, df2)  else  pf(d, df1, df2, ncp)  #p
  getQuantile <- function(d)
        if (no.ncp)  qf(d, df1, df2)  else  qf(d, df1, df2, ncp)  #q

  #titleStr <- paste("F (",
  #                  sym$nu, "1 = ", round(df1, 3), ", ",
  #                  sym$nu, "2 = ", round(df2, 3),
  #                  (if (!missing(ncp))
  #                    paste(",", sym$delta, "=", round(ncp, 3))),
  #                  ")", sep = "")
  if (!missing(ncp))
      titleStr <- bquote(bold("F ("~n[1]~"="~.(round(df1, 3))~","~n[2]~"="~.(round(df2, 3))~","~.(sym$lambda)~"="~.(round(ncp,3))~")"))
  else
      titleStr <- bquote(bold("F ("~n[1]~"="~.(round(df1, 3))~","~n[2]~"="~.(round(df2, 3))~")"))


  #############################################################################

  out <- PlotContinuous(
    u                = u,
    minPlotQuantile  = minPlotQuantile,
    maxPlotQuantile  = maxPlotQuantile,
    plot             = plot,
    showCDF          = showCDF,
    showPDF          = showPDF,
    showECDF         = showECDF,
    show             = show,
    maxInvPlotted    = maxInvPlotted,
    plotDelay        = plotDelay,
    sampleColor      = sampleColor,
    populationColor  = populationColor,
    showTitle        = showTitle,
    respectLayout    = respectLayout,
    getDensity       = getDensity,
    getDistro        = getDistro,
    getQuantile      = getQuantile,
    hasCDF           = !missing(showCDF),
    hasPDF           = !missing(showPDF),
    hasECDF          = !missing(showECDF),
    titleStr         = titleStr
  )

  # resetting par and warning settings
  options(warn = warnVal$warn)
  if (!all(oldpar$mfrow == par()$mfrow)) {
    # ?par claims "restoring all of [oldpar] is not wise", so reset only mfrow
    par(mfrow = oldpar$mfrow)
  }

  if (!is.null(out)) return(out)
}
