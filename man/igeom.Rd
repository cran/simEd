\name{igeom}
\alias{igeom}

\title{Random Variate Generation for the Geometric Distribution}
\description{
  Generates random variates from the geometric distribution by inversion.
  Optionally graphs the population cumulative distribution function and
  associated random variates, the population probability mass function
  and the empirical probability mass function of the random variates, and the
  empirical cumulative distribution function versus the population cumulative
  distribution function.
}
\usage{
  igeom(u = runif(1), prob, 
         minPlotQuantile  = 0,
         maxPlotQuantile  = 0.95,
         plot             = TRUE,
         showCDF          = TRUE,
         showPMF          = FALSE,
         showECDF         = FALSE,
         show             = NULL,
         plotDelay        = 0,
         maxPlotTime      = 30,
         resetRowsMargins = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{u}{vector of uniform(0,1) random numbers}
  \item{prob}{probability of success in each trial (0 \eqn{<} \code{prob} \eqn{\le} 1)}
  \item{minPlotQuantile}{controls the minimum quantile to appear in the plots}
  \item{maxPlotQuantile}{controls the maximum quantile to appear in the plots}
  \item{plot}{logical; if \code{TRUE} (default), one or more plots will appear (see parameters below); otherwise no plots appear}
  \item{showCDF}{logical; if \code{TRUE} (default), cdf plot appears, otherwise cdf plot is suppressed} 
  \item{showPMF}{logical; if \code{FALSE} (default), pmf plot is suppressed, otherwise pmf plot appears} 
  \item{showECDF}{logical; if \code{FALSE} (default), ecdf plot is suppressed, otherwise ecdf plot appears} 
  \item{show}{shortcut way of denoting plots to display; either a binary vector of length three or an integer in [0,7] (see "Details" below)}
  \item{plotDelay}{delay, in seconds, between generation of the random variates}
  \item{maxPlotTime}{maximum time, in seconds, to plot all of the random variates}
  \item{resetRowsMargins}{logical; if \code{TRUE} (default), resets the plot device to default rows and margins on exit}
}
\details{
Generates random variates from the geometric distribution, and optionally, illustrates
  \itemize{
    \item the use of the inverse-cdf technique,
    \item the effect of random sampling variability in relation to the pmf and cdf.
  }
  When all of the graphics are requested,
  \itemize{
    \item the top graph illustrates the use of the inverse-cdf technique by
        graphing the population cdf and the transformation of the random numbers
        to random variates, 
    \item the middle graph illustrates the effect of random sampling variability
        by graphing the population pmf and the empirical pmf associated with the
        random variates, and
    \item the bottom graph illustrates effect of random sampling variability by
        graphing the population cdf and the empirical cdf associated with the
        random variates.
  }
  All aspects of the random variate generation algorithm are output in red.  All
  aspects of the population distribution are output in black.

  The geometric distribution with parameter \code{prob} = \eqn{p} has density

           \deqn{p(x) = p (1-p)^x}

  for \eqn{x = 0, 1, 2, \ldots}, where \eqn{0 < p \le 1}.

  The algorithm for generating random variates from the geometric distribution is
  synchronized (one random variate for each random number) and monotone in u.
  This means that the variates generated here might be useful in some variance
  reduction techniques used in Monte Carlo and discrete-event simulation. 

  Values from the u vector are plotted in the cdf plot along the vertical axis
  as red dots.  A horizontal, dashed, red line extends from the red dot to the
  population cdf.  At the intersection, a vertical, dashed red line extends
  downward to the horizontal axis, where a second red dot, denoting the
  associated geometric random variate is plotted.

  This is not a particularly fast variate generation algorithm because it uses
  the base R \code{qgeom} function to invert the values contained in \code{u}.

  All of the elements of the \code{u} vector must be between 0 and 1.
  Alternatively, \code{u} can be \code{NULL} in which case plot(s) of the
  theoretical pmf and cdf are displayed according to plotting parameter values
  (defaulting to display of both the pmf and cdf).

  The \code{show} parameter can be used as a shortcut way to denote plots to
  display.  The argument to \code{show} can be either:
  \itemize{
    \item a binary vector of length three, where the entries from left to right
        correspond to \code{showCDF}, \code{showPMF}, and \code{showECDF},
        respectively.  For each entry, a 1 indicates the plot should be
        displayed, and a 0 indicates the plot should be suppressed.
    \item an integer in [0,7] interpreted similar to Unix's chmod command.  That
        is, the integer's binary representation can be transformed into a
        length-three vector discussed above (e.g., 6 corresponds to c(1,1,0)).
        See examples.
  }
  Any valid value for \code{show} takes precedence over existing individual
  values for \code{showCDF}, \code{showPMF}, and \code{showECDF}.

  The \code{minPlotQuantile} and \code{maxPlotQuantile} arguments are present in
  order to compress the plots horizontally.   The random variates generated are
  not impacted by these two arguments.  Vertical, dotted, black lines are
  plotted at the associated quantiles on the plots.
    
  The \code{plotDelay} and \code{maxPlotTime} arguments can be used to slow down
  the variate generation for classroom explanation.

  In the plot associated with the pmf, the maximum plotting height is associated
  with 125\% of the maximum height of pmf.  Any histogram cell that extends
  above this limit will have three black dots appearing above it. 
}
\value{
  A vector of geometric random variates. 
}
\author{Barry Lawson (\email{blawson@richmond.edu}), 
        Larry Leemis (\email{leemis@math.wm.edu})}
\examples{
igeom(0.7, 0.4)

set.seed(8675309)
igeom(runif(10), 0.4, showPMF = TRUE)

set.seed(8675309)
igeom(runif(10), 0.4, showPMF = TRUE, showCDF = FALSE)

igeom(runif(120), 0.4, showPMF = TRUE, minPlotQuantile = 0.02, maxPlotQuantile = 0.98)

# show the PMF and CDF without any variates
igeom(NULL, 0.4)

# show CDF with inversion and PMF using show
igeom(runif(10), 0.4, show = c(1,1,0))
igeom(runif(10), 0.4, show = 6)

# show CDF with inversion and ECDF using show
igeom(runif(10), 0.4, show = c(1,0,1))
igeom(runif(10), 0.4, show = 5)

# show CDF with inversion, PMF, and ECDF using show
igeom(runif(10), 0.4, show = c(1,1,1))
igeom(runif(10), 0.4, show = 7)

}
\keyword{hplot,dynamic,distribution}  % RShowDoc("KEYWORDS") for suggested list
\concept{random variate generation}
