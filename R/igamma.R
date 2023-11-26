################################################################################
#  igamma - IDF Visualization Function for Gamma Distribution
# ------------------------------------------------------------------------------
#  The igamma R function visualizes the F idf evaluated at a provided
#  uniform(0,1) u. This will graph the idf in action (via dashed lines back across
#  the cdf). Note that the u argument can be a scalar or vector.  If a vector,
#  multiple dashed lines will be displayed, and the return type will be a vector.
#  The function also gives the option of displaying a histogram of the variates
#  generated, with theoretical Gamma superimposed.
# ------------------------------------------------------------------------------
#' @templateVar distro   Gamma
#' @templateVar distrolc gamma
#' @templateVar ifunct   igamma
#' @templateVar funct    gamma
#' @templateVar PXF      PDF
#' @templateVar massDen  density
#' @templateVar arglong  shape = 5, scale = 3
#' @templateVar argshort 3, 2
#' @templateVar minPQ    0
#' @templateVar maxPQ    0.95
#'
#' @template i-cont
#' @template -gamma
#' @template i-2
#' @examples
#'   # overlay visual exploration of ks.test results
#'   oldpar <- par(no.readonly = TRUE)
#'   set.seed(54321)
#'   vals <- igamma(runif(10), 3, 2, showECDF = TRUE, restorePar = FALSE)
#'   D <- as.numeric(ks.test(vals, "pgamma", 3, 2)$statistic)
#'   for (x in seq(1.20, 1.60, by = 0.05)) {
#'     y <- pgamma(x, 3, 2)
#'     segments(x, y, x, y + D, col = "darkgreen", lwd = 2, xpd = NA)
#'   }
#'   par(oldpar) # restore original par values, since restorePar = FALSE above
#'
#' @export
################################################################################
igamma <- function(u = runif(1), shape, rate = 1, scale = 1/rate,
                  minPlotQuantile = 0,
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

  if (!missing(rate) && !missing(scale) && rate != 1/scale)
    stop("specify 'rate' or 'scale' but not both")

  checkVal(shape, minex = 0)
  if (!missing(rate))   checkVal(rate,  minex = 0)
  if (!missing(scale))  checkVal(scale, minex = 0)

  checkQuants(minPlotQuantile, maxPlotQuantile, min = 0, maxex = 1)

  #options(warn = 1)                   # set to immediate warnings (del 22 Nov 2023)

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

  # Define getter functions
  getDensity  <- function(d)  dgamma(d, shape = shape, scale = scale)  #d
  getDistro   <- function(d)  pgamma(d, shape = shape, scale = scale)  #p
  getQuantile <- function(d)  qgamma(d, shape = shape, scale = scale)  #q

  # using plotmath for alpha, beta, theta; bquote to use .() to evaluate args;
  #  in bquote, ~ includes space b/w while * appends w/ no space b/w
  if (missing(rate)) {
    titleStr <- as.expression(bquote(bold(
                    "Gamma (" ~ "k"   ~ "=" ~ .(round(shape, 3)) * ","
                              ~ theta ~ "=" ~ .(round(scale, 3)) ~ ")"
                )))
  } else {
    # see notation on Wikipedia: https://en.wikipedia.org/wiki/Gamma_distribution
    titleStr <- as.expression(bquote(bold(
                    "Gamma (" ~ alpha ~ "=" ~ .(round(shape, 3)) * ","
                              ~ beta  ~ "=" ~ .(round(scale, 3)) ~ ")"
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
  #options(warn = warnVal$warn)  # remove RE CRAN req't (del 22 Nov 2023)

  ## using on.exit for par RE CRAN suggest (del 22 Nov 2023)
  #if (!all(oldpar$mfrow == par()$mfrow)) {
  #  # ?par claims "restoring all of [oldpar] is not wise", so reset only mfrow
  #  par(mfrow = oldpar$mfrow)
  #}

  if (!is.null(out)) return(out)
}
