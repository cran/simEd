\name{iexp}
\alias{iexp}

\title{Random Variate Generation for the Exponential Distribution}
\description{
  Generates random variates from the exponential distribution by inversion.
  Optionally graphs the population cumulative distribution function and
  associated random variates, the population probability density function
  and a histogram of the random variates, and the empirical cumulative
  distribution function versus the population cumulative distribution function.
}
\usage{
  iexp(u = runif(1), rate = 1, 
         minPlotQuantile = 0,
         maxPlotQuantile = 0.95,
         plot            = TRUE,
         showCDF         = TRUE,
         showPDF         = FALSE,
         showECDF        = FALSE,
         show            = NULL,
         plotDelay       = 0,
         maxPlotTime     = 30,
         showTitle       = TRUE,
         respectLayout   = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{u}{vector of uniform(0,1) random numbers, or NULL}
  \item{rate}{rate parameter;  must be positive}
  \item{minPlotQuantile}{controls the minimum quantile to appear in the plots}
  \item{maxPlotQuantile}{controls the maximum quantile to appear in the plots}
  \item{plot}{logical; if \code{TRUE} (default), one or more plots will appear (see parameters below); otherwise no plots appear}
  \item{showCDF}{logical; if \code{TRUE} (default), cdf plot appears, otherwise cdf plot is suppressed} 
  \item{showPDF}{logical; if \code{FALSE} (default), pdf plot is suppressed, otherwise pdf plot appears} 
  \item{showECDF}{logical; if \code{FALSE} (default), ecdf plot is suppressed, otherwise ecdf plot appears} 
  \item{show}{shortcut way of denoting plots to display; either a binary vector of length three or an integer in [0,7] (see "Details" below)}
  \item{plotDelay}{delay, in seconds, between generation of the random variates}
  \item{maxPlotTime}{maximum time, in seconds, to plot all of the random variates}
  \item{showTitle}{logical; if \code{TRUE} (default), displays a title in the first of any displayed plots}
  \item{respectLayout}{logical; if \code{TRUE} (default), respects existing settings for device layout (see "Details" below)}
}
\details{
Generates random variates from the exponential distribution, and optionally, illustrates
  \itemize{
    \item the use of the inverse-cdf technique,
    \item the effect of random sampling variability in relation to the pdf and cdf.
  }
  When all of the graphics are requested,
  \itemize{
    \item the first graph illustrates the use of the inverse-cdf technique by
        graphing the population cdf and the transformation of the random numbers
        to random variates, 
    \item the second graph illustrates the effect of random sampling variability
        by graphing the population pdf and the histogram associated with the
        random variates, and
    \item the third graph illustrates effect of random sampling variability by
        graphing the population cdf and the empirical cdf associated with the
        random variates.
  }
  All aspects of the random variate generation algorithm are output in red.  All
  aspects of the population distribution are output in black.

  The exponential distribution with rate \eqn{\lambda} has density

           \deqn{f(x) = \lambda e^{-\lambda x}}{%
                 f(x) = \lambda e^(-\lambda x)}
           
  for \eqn{x \ge 0}.

  The algorithm for generating random variates from the exponential distribution is
  synchronized (one random variate for each random number) and monotone in u.
  This means that the variates generated here might be useful in some variance
  reduction techniques used in Monte Carlo and discrete-event simulation. 

  Values from the u vector are plotted in the cdf plot along the vertical axis
  as red dots.  A horizontal, dashed, red line extends from the red dot to the
  population cdf.  At the intersection, a vertical, dashed red line extends
  downward to the horizontal axis, where a second red dot, denoting the
  associated exponential random variate is plotted.

  This is not a particularly fast variate generation algorithm because it uses
  the base R \code{qexp} function to invert the values contained in \code{u}.

  All of the elements of the \code{u} vector must be between 0 and 1.
  Alternatively, \code{u} can be \code{NULL} in which case plot(s) of the
  theoretical pdf and cdf are displayed according to plotting parameter values
  (defaulting to display of both the pdf and cdf).

  The \code{show} parameter can be used as a shortcut way to denote plots to
  display.  The argument to \code{show} can be either:
  \itemize{
    \item a binary vector of length three, where the entries from left to right
        correspond to \code{showCDF}, \code{showPDF}, and \code{showECDF},
        respectively.  For each entry, a 1 indicates the plot should be
        displayed, and a 0 indicates the plot should be suppressed.
    \item an integer in [0,7] interpreted similar to Unix's chmod command.  That
        is, the integer's binary representation can be transformed into a
        length-three vector discussed above (e.g., 6 corresponds to c(1,1,0)).
        See examples.
  }
  Any valid value for \code{show} takes precedence over existing individual
  values for \code{showCDF}, \code{showPDF}, and \code{showECDF}.

  If \code{respectLayout} is \code{TRUE}, the function respects existing
  settings for device layout.  Note, however, that if the number of plots
  requested (either via \code{show} or via \code{showCDF}, \code{showPMF}, and
  \code{showECDF}) exceeds the number of plots available in the current layout
  (as determined by \code{prod(par("mfrow"))}), the function will display all
  requested plots but will also display a warning message indicating that the
  current layout does not permit simultaneous viewing of all requested plots.

  If \code{respectLayout} is \code{FALSE}, any existing user settings for device
  layout are ignored.  That is, the function uses \code{par} to explicitly set
  \code{mfrow} sufficient to show all requested plots stacked vertically to
  align their horizontal axes, and then resets row, column, and margin settings
  to R default values on exit.

  The \code{minPlotQuantile} and \code{maxPlotQuantile} arguments are present in
  order to compress the plots horizontally.   The random variates generated are
  not impacted by these two arguments.  Vertical, dotted, black lines are
  plotted at the associated quantiles on the plots.
    
  The \code{plotDelay} and \code{maxPlotTime} arguments can be used to slow down
  the variate generation for classroom explanation.

  In the plot associated with the pdf, the maximum plotting height is associated
  with 125\% of the maximum height of pdf.  Any histogram cell that extends
  above this limit will have three black dots appearing above it. 
}
\value{
  A vector of exponential random variates 
}
\author{Barry Lawson (\email{blawson@richmond.edu}),
        Larry Leemis (\email{leemis@math.wm.edu})}
\examples{
iexp(0.7, rate = 2)

par(mfrow = c(2,1)) # 2 rows, 1 col for CDF and PDF plots
set.seed(8675309)
iexp(runif(10), rate = 2, showPDF = TRUE)

par(mfrow = c(2,1)) # 2 rows, 1 col for CDF and ECDF plots
set.seed(8675309)
iexp(runif(10), rate = 2, showECDF = TRUE)

par(mfrow = c(3,1)) # 3 rows, 1 col for CDF, PDF, and ECDF plots
set.seed(8675309)
iexp(runif(10), rate = 2, showPDF = TRUE, showECDF = TRUE)

par(mfrow = c(1,1)) # 1 row, 1 col for PDF only plot
set.seed(8675309)
iexp(runif(10), rate = 2, showPDF = TRUE, showCDF = FALSE)

par(mfrow = c(2,1)) # 2 rows, 1 col for CDF and PDF plots
iexp(runif(120), rate = 2, showPDF = TRUE, minPlotQuantile = 0.02, maxPlotQuantile = 0.98)

# overlay visual exploration of ks.test results
par(mfrow = c(2,1)) # 2 rows, 1 col for CDF and ECDF plots
set.seed(54321)
vals <- iexp(runif(10), rate = 2, showECDF = TRUE)
D <- as.numeric(ks.test(vals, "pexp", 2)$statistic)
for (x in seq(0.05, 0.75, by = 0.05)) {
   y <- pexp(x, rate = 2)
   segments(x, y, x, y + D, col = "darkgreen", lwd = 2, xpd = NA)
}

# plot the PDF and CDF without any variates
par(mfrow = c(2,1)) # 2 rows, 1 col for PDF and CDF plots
iexp(NULL, rate = 2)

# plot CDF with inversion and PDF using show
par(mfrow = c(2,1)) # 2 rows, 1 col for CDF and PDF plots
iexp(runif(10), rate = 2, show = c(1,1,0))
iexp(runif(10), rate = 2, show = 6)

# plot CDF with inversion and ECDF using show
par(mfrow = c(2,1)) # 2 rows, 1 col for CDF and ECDF plots
iexp(runif(10), rate = 2, show = c(1,0,1))
iexp(runif(10), rate = 2, show = 5)

# plot CDF with inversion, PDF, and ECDF using show
par(mfrow = c(3,1)) # 3 rows, 1 col for CDF, PDF, and ECDF plots
iexp(runif(10), rate = 2, show = c(1,1,1))
iexp(runif(10), rate = 2, show = 7)

# plot three different CDF+PDF+ECDF vertical displays
par(mfcol = c(3,3))  # 3 rows, 3 cols, filling columns before rows
set.seed(8675309)
iexp(runif(20), rate = 2, show = 7)
iexp(runif(20), rate = 2, show = 7)
iexp(runif(20), rate = 2, show = 7)

# plot three different CDF+PDF+ECDF horizontal displays, with title only
# on the first display
par(mfrow = c(3,3))  # 3 rows, 3 cols, filling rows before columns
set.seed(8675309)
iexp(runif(20), rate = 2, show = 7)
iexp(runif(20), rate = 2, show = 7, showTitle = FALSE)
iexp(runif(20), rate = 2, show = 7, showTitle = FALSE)

# exhibit use of the respectLayout = FALSE option
par(mfrow = c(3,3))  # this will be ignored below since respectLayout = FALSE
set.seed(8675309)
iexp(runif(20), rate = 2, show = 7, respectLayout = FALSE)
par("mfrow")  # will have been reset c(1,1)
par("mar")    # will have been reset to c(5.1, 4.1, 4.1, 2.1)

}
\keyword{hplot,dynamic,distribution}  % RShowDoc("KEYWORDS") for suggested list
\concept{random variate generation}
