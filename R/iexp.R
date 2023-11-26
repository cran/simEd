################################################################################
#  iexp - IDF Visualization Function for Exponential Distribution
# ------------------------------------------------------------------------------
#  The iexp R function visualizes the exponential idf evaluated at a provided
#  uniform(0,1) u. This will graph the idf in action (via dashed lines back across
#  the cdf). Note that the u argument can be a scalar or vector.  If a vector,
#  multiple dashed lines will be displayed, and the return type will be a vector.
#  The function also gives the option of displaying a histogram of the variates
#  generated, with theoretical exponential distribution superimposed.
# ------------------------------------------------------------------------------
#' @templateVar distro   Exponential
#' @templateVar distrolc exponential
#' @templateVar ifunct   iexp
#' @templateVar funct    exp
#' @templateVar PXF      PDF
#' @templateVar massDen  density
#' @templateVar arglong  rate = 3
#' @templateVar argshort 2
#' @templateVar minPQ    0
#' @templateVar maxPQ    0.99
#'
#' @template i-cont
#' @template -exp
#' @template i-2
#' @examples
#'   # overlay visual exploration of ks.test results
#'   oldpar <- par(no.readonly = TRUE)
#'   set.seed(54321)
#'   vals <- iexp(runif(10), 2, showECDF = TRUE, restorePar = FALSE)
#'   D <- as.numeric(ks.test(vals, "pexp", 2)$statistic)
#'   for (x in seq(0.25, 0.65, by = 0.05)) {
#'     y <- pexp(x, 2)
#'     segments(x, y, x, y + D, col = "darkgreen", lwd = 2, xpd = NA)
#'   }
#'   par(oldpar) # restore original par values, since restorePar = FALSE above
#'
#' @export
################################################################################
iexp <- function(u = runif(1), rate = 1,
                  minPlotQuantile = 0,
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

  checkVal(rate, minex = 0)
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
  getDensity  <- function(d)  dexp(d, rate)  #d
  getDistro   <- function(d)  pexp(d, rate)  #p
  getQuantile <- function(d)  qexp(d, rate)  #q

  # using plotmath for lambda; bquote to use .() to evaluate args;
  #  in bquote, ~ includes space b/w while * appends w/ no space b/w
  titleStr <- as.expression(bquote(bold(
                    "Exponential (" ~ lambda ~ "=" ~ .(round(rate, 3)) ~ ")"
              )))

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
