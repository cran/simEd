
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simEd (Simulation Education)

<!-- badges: start -->

[![R-CMD-check](https://github.com/blawson-bates/simEd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/blawson-bates/simEd/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This package contains various functions to be used for simulation
education, including: simple Monte Carlo simulation functions; queueing
simulation functions with optional animation; variate generation
functions capable of producing independent streams and antithetic
variates; separate functions for visualizing/animating (a) event-driven
simulation details of a single-server queue model, (b) a Lehmer
random-number generator, (c) random variate generation via
acceptance-rejection, (d) generation of a non-homogeneous Poisson
process via thinning, and (e) random variate generation for various
discrete and continuous distributions; and functions to compute
time-persistent statistics. The package also contains two queueing data
sets (one fabricated, one real-world) to facilitate input modeling.

## Example

This is an example showing use of the `ssq` function in our package to
simulate a simple M/M/1 queue, passing in a custom exponential
interarrival function defined using our `vexp` variate generator, and
then plotting the number in the system across time, with superimposed
time-averaged statistics computed using `meanTPS` and `sdTPS`:

``` r
## ssq example code
library(simEd)
myArrFcn <- function() { vexp(1, rate = 1 / 0.95, stream = 1) }
output <- ssq(maxArrivals = 100, seed = 8675309, interarrivalFcn = myArrFcn,
              saveNumInSystem = TRUE, showOutput = FALSE)
avg <- meanTPS(output$numInSystemT, output$numInSystemN)
sd <- sdTPS(output$numInSystemT, output$numInSystemN)
plot(output$numInSystemT, output$numInSystemN, type = "s", main = "M/M/1 Queue",
     bty = "l", las = 1, xlab = "time", ylab = "number in system")
abline(h = avg, lwd = 2, col = "red")
abline(h = c(avg - sd, avg + sd), lwd = 2, lty = "dotted", col = "red")
```

## Installation

Install the current version of `simEd` from CRAN using
`install.packages("simEd")`.

Note that the `simEd` package depends on Josef Leydold’s `rstream`
package, a wrapper of Pierre L’Ecuyer’s “mrg32k3a” random number
generator, to provide independent streams of uniform(0,1) random
numbers. The `simEd` package also depends on the `shape` package, used
in producing animations. If either of the `rstream` or `shape` package
is not already installed, the previous step will install them
automatically.

## Details

The goal of this package is to facilitate use of R for an introductory
course in discrete-event simulation.

This package contains animation functions for visualizing:

- event-driven details of a single-server queue model: `ssqvis`;
- a Lehmer random number generator: `lehmer`;
- variate generation via acceptance-rejection: `accrej`;
- generation of a non-homogeneous Poisson process via thinning:
  `thinning`.

This package contains variate generators capable of independent streams
(based on Josef Leydold’s `rstream` package) and antithetic variates for
four discrete and eleven continuous distributions:

- discrete: `vbinom`, `vgeom`, `vnbinom`, `vpois`,
- continuous: `vbeta`, `vcauchy`, `vchisq`, `vexp`, `vgamma`, `vlnorm`,
  `vlogis`, `vnorm`, `vt`, `vunif`, `vweibull`

All of the variate generators use inversion, and are therefore monotone
and synchronized.

The package contains functions to visualize variate generation for the
same four discrete and eleven continuous distributions:

- discrete: `ibinom`, `igeom`, `inbinom`, `ipois`,
- continuous: `ibeta`, `icauchy`, `ichisq`, `iexp`, `igamma`, `ilnorm`,
  `ilogis`, `inorm`, `it`, `iunif`, `iweibull`

The package contains functions that implement Monte Carlo simulation
approaches for estimating probabilities in two different dice games:

- Galileo’s dice problem: `galileo`
- craps: `craps`

The package also contains functions that are event-driven simulation
implementations of a single-server single-queue system and of a
multiple-server single-queue system:

- single-server: `ssq`
- multiple-server: `msq`

Both queueing functions are extensible in allowing the user to provide
custom arrival and service process functions. Both functions provide
animation.

The package contains four functions primarily for visualizing simulation
concepts:

- event-driven details of a single-server queuing system: `ssqvis`
- Lehmer random number generator: `lehmer`
- variate generation via acceptance-rejection: `accrej`
- generating a non-homogeneous Poisson process via thinning: `thinning`

The package contains three functions for computing time-persistent
statistics:

- time-average mean: `meanTPS`
- time-average standard deviation: `sdTPS`
- time-average quantiles: `quantileTPS`

The package also masks two functions from the `stats` package:

- `set.seed`, which explicitly calls the `stats` version in addition to
  setting up seeds for the independent streams in the package;
- `sample`, which provides capability to use independent streams and
  antithetic variates.

Finally, the package provides two queueing data sets to facilitate input
modeling:

- `queueTrace`, which contains 1000 arrival times and 1000 service times
  (all fabricated) for a single-server queueing system;
- `tylersGrill`, which contains 1434 arrival times and 110 (sampled)
  service times corresponding to actual data collected during one
  business day at Tyler’s Grill at the University of Richmond.
