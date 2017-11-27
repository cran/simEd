\name{simEd-package}
\alias{simEd-package}
\alias{simEd}
\docType{package}

\title{
  \packageTitle{simEd}
}

\description{
  \packageDescription{simEd}

  \strong{Request From Authors:}
  If you adopt and use this package for your simulation course, we would
  greatly appreciate were you to email us (addresses below) to let us know, as
  we would like to maintain a list of adopters.  Please include your name,
  university/affiliation, and course name/number.  Thanks!
}
\details{
   The goal of this package is to facilitate use of R for an introductory
   course in discrete-event simulation.

   This package contains variate generators capable of independent streams
   (based on Josef Leydold's \code{\link{rstream}} package) and antithetic
   variates for two discrete and five continuous distributions:
   \itemize{
      \item discrete: \code{\link{vbinom}}, \code{\link{vgeom}}
      \item continuous: \code{\link{vexp}}, \code{\link{vgamma}},
         \code{\link{vnorm}}, \code{\link{vunif}}, \code{\link{vweibull}}
   }
   All of the variate generators use inversion, and are therefore monotone and
   synchronized.

   The package contains functions to visualize variate generation for the same
   two discrete and five continuous distributions:
   \itemize{
      \item discrete: \code{\link{ibinom}}, \code{\link{igeom}}
      \item continuous: \code{\link{iexp}}, \code{\link{igamma}},
         \code{\link{inorm}}, \code{\link{iunif}}, \code{\link{iweibull}}
   }
   
   The package contains functions that implement Monte Carlo simulation
   approaches for estimating probabilities in two different dice games:
   \itemize{
      \item Galileo's dice problem: \code{\link{galileo}}
      \item craps: \code{\link{craps}}
   }

   The package also contains functions that are event-driven simulation
   implementations of a single-server single-queue system and of a
   multiple-server single-queue system:
   \itemize{
      \item single-server: \code{\link{ssq}}
      \item multiple-server: \code{\link{msq}}
   }
   Both queueing functions are extensible in allowing the user to provide custom
   arrival and service process functions.

   The package contains three functions for computing time-persistent statistics:
   \itemize{
      \item time-average mean: \code{\link{meanTPS}}
      \item time-average standard deviation: \code{\link{sdTPS}}
      \item time-average quantiles: \code{\link{quantileTPS}}
   }
   
   The package also masks two functions from the \code{\link{stats}} package:
   \itemize{
      \item \code{\link{set.seed}}, which explicitly calls the \code{\link{stats}}
         version in addition to setting up seeds for the independent streams in
         the package;
      \item \code{\link{sample}}, which provides capability to use independent
        streams and antithetic variates.
   }

   Finally, the package provides two queueing data sets to facilitate input
   modeling:
   \itemize{
      \item \code{\link{queueTrace}}, which contains 1000 arrival times and
          1000 service times (all fabricated) for a single-server queueing
          system;
      \item \code{\link{tylersGrill}}, which contains 1434 arrival times and
          110 (sampled) service times corresponding to actual data collected
          during one business day at Tyler's Grill at the University of Richmond.
   }
}
\section{Acknowledgments}{
   The authors would like to thank Dr. Barry L. Nelson, Walter P. Murphy
   Professor in the Department of Industrial Engineering & Management Sciences
   at Northwestern University, for meaningful feedback during the development of
   this package.
}
\author{
\packageAuthor{simEd}

Maintainer: \packageMaintainer{simEd}
}
\keyword{ package }
