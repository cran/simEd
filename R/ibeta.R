################################################################################
#  ibeta - IDF Visualization Function for Beta Distribution
# ------------------------------------------------------------------------------
#  The ibeta R function visualizes the beta idf evaluated at a provided
#  uniform(0,1) u. This will graph the idf in action (via dashed lines back across
#  the cdf). Note that the u argument can be a scalar or vector.  If a vector,
#  multiple dashed lines will be displayed, and the return type will be a vector.
#  The function also gives the option of displaying a histogram of the variates
#  generated, with theoretical beta superimposed.
# ------------------------------------------------------------------------------
#' @templateVar distro   Beta
#' @templateVar distrolc beta
#' @templateVar ifunct   ibeta
#' @templateVar funct    beta
#' @templateVar PXF      PDF
#' @templateVar massDen  density
#' @templateVar arglong  shape1 = 3, shape2 = 1, ncp = 2
#' @templateVar argshort 3, 1
#' @templateVar minPQ    0.01
#' @templateVar maxPQ    0.95
#'
#' @template i-cont
#' @template -beta
#' @template i-2
#' @export
################################################################################
ibeta <- function (u = runif(1), shape1, shape2, ncp = 0,
                minPlotQuantile = 0.01,
                maxPlotQuantile = 0.95,
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
                respectLayout   = FALSE, 
                restorePar      = TRUE,  # add 23 Nov 2023
                ...)
{
  #############################################################################

  if(is.null(dev.list()))  dev.new(width=5, height=6)
  
  #warnVal <- options("warn")          # save current warning setting... (del 22 Nov 2023)

  #############################################################################

  #options(warn = -1)          # suppress warnings -- remove RE CRAN req't (del 22 Nov 2023)

  if (!is.null(u) && (min(u) <= 0 || max(u) >= 1))  stop("must have 0 < u < 1")
  if (length(u) == 0)  u <- NULL

  checkVal(shape1, minex = 0)
  checkVal(shape2, minex = 0)
  if (!missing(ncp))
    checkVal(ncp, min = 0, define = "non-centrality param")

  checkQuants(minPlotQuantile, maxPlotQuantile, min = 0, max = 1)

  # options(warn = 1)                   # set to immediate warnings (del 22 Nov 2023)

  # Check for deprecated parameters
  for (arg in names(list(...))) {
    if (arg == "maxPlotTime") {
      # mod 23 Nov 2023
      #warning("'maxPlotTime' has been deprecated as of simEd v2.0.0")
      warning("'maxPlotTime' has been deprecated as of simEd v2.0.0",
              immediate. = TRUE)
    }
    else stop(paste("Unknown argument '", arg, "'", sep = ""))
  }

  #############################################################################

  no.ncp <- missing(ncp)

  # Define getter functions
  getDensity  <- function(d)
    if (no.ncp) dbeta(d,shape1,shape2) else dbeta(d,shape1,shape2,ncp) #d
  getDistro   <- function(d)
    if (no.ncp) pbeta(d,shape1,shape2) else pbeta(d,shape1,shape2,ncp) #p
  getQuantile <- function(d)
    if (no.ncp) qbeta(d,shape1,shape2) else qbeta(d,shape1,shape2,ncp) #q

  # [https://www.mail-archive.com/r-package-devel@r-project.org/msg09126.html]
  # using plotmath for alpha, beta, Delta; bquote to use .() to evaluate args;
  #  in bquote, ~ includes space b/w while * appends w/ no space b/w
  if (no.ncp) {
      titleStr <- as.expression(bquote(bold(
                   "Beta (" ~ alpha ~ "=" ~ .(round(shape1, 3)) * ","
                            ~ beta  ~ "=" ~ .(round(shape2, 3)) ~ ")"
                  )))
  } else {
      titleStr <- as.expression(bquote(bold(
                   "Beta (" ~ alpha ~ "=" ~ .(round(shape1, 3)) * ","
                            ~ beta  ~ "=" ~ .(round(shape2, 3)) * ","
                            ~ Delta ~ "=" ~ .(round(ncp,    3)) ~ ")"
                   )))
  }


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
    restorePar       = restorePar,    # add 23 Nov 2023
    getDensity       = getDensity,
    getDistro        = getDistro,
    getQuantile      = getQuantile,
    hasCDF           = !missing(showCDF),
    hasPDF           = !missing(showPDF),
    hasECDF          = !missing(showECDF),
    titleStr         = titleStr
  )

  # resetting par and warning settings
  #options(warn = warnVal$warn) -- remove RE CRAN req't (del 22 Nov 2023)

  ## using on.exit for par RE CRAN suggest (del 22 Nov 2023)
  #if (!all(oldpar$mfrow == par()$mfrow)) {
  #  # ?par claims "restoring all of [oldpar] is not wise", so reset only mfrow
  #  par(mfrow = oldpar$mfrow)
  #}

  if (!is.null(out)) return(out)
}
