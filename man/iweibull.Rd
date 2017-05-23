\name{iweibull}
\alias{iweibull}

\title{Random Variate Generation for the Weibull Distribution}
\description{
  Generates random variates from the Weibull distribution by inversion.
  Optionally graphs the population cumulative distribution function and
  associated random variates, the population probability density function
  and a histogram of the random variates, and the empirical cumulative
  distribution function versus the population cumulative distribution function.
}
\usage{
  iweibull(u = runif(1), shape, scale = 1,
         minPlotQuantile  = 0,
         maxPlotQuantile  = 0.99,
         plot             = TRUE,
         showCDF          = TRUE,
         showPDF          = FALSE,
         showECDF         = FALSE,
         show             = NULL,
         plotDelay        = 0,
         maxPlotTime      = 30,
         resetRowsMargins = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{u}{vector of uniform(0,1) random numbers, or NULL}
  \item{shape, scale}{shape and scale parameters (must be positive)}
  \item{minPlotQuantile}{controls the minimum quantile to appear in the plots}
  \item{maxPlotQuantile}{controls the maximum quantile to appear in the plots}
  \item{plot}{logical; if \code{TRUE} (default), one or more plots will appear (see parameters below); otherwise no plots appear}
  \item{showCDF}{logical; if \code{TRUE} (default), cdf plot appears, otherwise cdf plot is suppressed} 
  \item{showPDF}{logical; if \code{FALSE} (default), pdf plot is suppressed, otherwise pdf plot appears} 
  \item{showECDF}{logical; if \code{FALSE} (default), ecdf plot is suppressed, otherwise ecdf plot appears} 
  \item{show}{shortcut way of denoting plots to display; either a binary vector of length three or an integer in [0,7] (see "Details" below)}
  \item{plotDelay}{delay, in seconds, between generation of the random variates}
  \item{maxPlotTime}{maximum time, in seconds, to plot all of the random variates}
  \item{resetRowsMargins}{logical; if \code{TRUE} (default), resets the plot device to default rows and margins on exit}
}
\details{
Generates random variates from the Weibull distribution, and optionally, illustrates
  \itemize{
    \item the use of the inverse-cdf technique,
    \item the effect of random sampling variability in relation to the pdf and cdf.
  }
  When all of the graphics are requested,
  \itemize{
    \item the top graph illustrates the use of the inverse-cdf technique by
        graphing the population cdf and the transformation of the random numbers
        to random variates, 
    \item the middle graph illustrates the effect of random sampling variability
        by graphing the population pdf and the histogram associated with the
        random variates, and
    \item the bottom graph illustrates effect of random sampling variability by
        graphing the population cdf and the empirical cdf associated with the
        random variates.
  }
  All aspects of the random variate generation algorithm are output in red.  All
  aspects of the population distribution are output in black.

  The Weibull distribution with parameters \code{shape} = \eqn{a} and 
  \code{scale} = \eqn{b} has density

      \deqn{f(x) = \frac{a}{b} \left(\frac{x}{b}\right)^{a-1} e^{-(x/b)^a}}{%
            f(x) = (a/b) (x/b)^(a-1) exp(-(x/b)^a)}

   for \eqn{x \ge 0}, \eqn{a > 0}, and \eqn{s > 0}.

  The algorithm for generating random variates from the Weibull distribution is
  synchronized (one random variate for each random number) and monotone in u.
  This means that the variates generated here might be useful in some variance
  reduction techniques used in Monte Carlo and discrete-event simulation. 

  Values from the u vector are plotted in the cdf plot along the vertical axis
  as red dots.  A horizontal, dashed, red line extends from the red dot to the
  population cdf.  At the intersection, a vertical, dashed red line extends
  downward to the horizontal axis, where a second red dot, denoting the
  associated Weibull random variate is plotted.

  This is not a particularly fast variate generation algorithm because it uses
  the base R \code{qweibull} function to invert the values contained in \code{u}.

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
  A vector of Weibull random variates 
}
\author{Barry Lawson (\email{blawson@richmond.edu}),
        Larry Leemis (\email{leemis@math.wm.edu})}
\examples{
iweibull(0.7, 3, 2)

set.seed(8675309)
iweibull(runif(10), 3, 2, showPDF = TRUE)

set.seed(8675309)
iweibull(runif(10), 3, 2, showECDF = TRUE)

set.seed(8675309)
iweibull(runif(10), 3, 2, showPDF = TRUE, showECDF = TRUE)

set.seed(8675309)
iweibull(runif(10), 3, 2, showPDF = TRUE, showCDF = FALSE)

iweibull(runif(120), 3, 2, showPDF = TRUE, minPlotQuantile = 0.02, maxPlotQuantile = 0.98)

# use resetRowsMargins to allow visual exploration of ks.test results
set.seed(54321)
vals <- iweibull(runif(10), 3, 2, showECDF = TRUE, resetRowsMargins = FALSE)
D <- as.numeric(ks.test(vals, "pweibull", 3, 2)$statistic)
for (x in seq(1.5, 2.0, by = 0.025)) {
   y <- pweibull(x, 3, 2)
   segments(x, y, x, y + D, col = "darkgreen", lwd = 2, xpd = NA)
}

# show the PDF and CDF without any variates
iweibull(NULL, 3, 2)

# show CDF with inversion and PDF using show
iweibull(runif(10), 3, 2, show = c(1,1,0))
iweibull(runif(10), 3, 2, show = 6)

# show CDF with inversion and ECDF using show
iweibull(runif(10), 3, 2, show = c(1,0,1))
iweibull(runif(10), 3, 2, show = 5)

# show CDF with inversion, PDF, and ECDF using show
iweibull(runif(10), 3, 2, show = c(1,1,1))
iweibull(runif(10), 3, 2, show = 7)

}
\keyword{hplot,dynamic,distribution}  % RShowDoc("KEYWORDS") for suggested list
\concept{random variate generation}
