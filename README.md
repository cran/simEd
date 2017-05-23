<!-- README.md is generated from README.Rmd. Please edit that file -->
simEd (Simulation Education)
============================

Contains various functions to be used for simulation education, including queueing simulation functions, variate generation functions capable of producing independent streams and antithetic variates, functions for illustrating random variate generation for various discrete and continuous distributions, and functions to compute time-persistent statistics. Also contains two queueing data sets (one fabricated, one real-world) to facilitate input modeling.

**Request From Authors**: If you adopt and use this package for your simulation course, we would greatly appreciate were you to email us (`blawson<at>richmond<dot>edu` or `leemis<at>math<dot>wm<dot>edu`) to let us know, as we would like to maintain a list of adopters. Please include your name, university/affiliation, and course name/number. Thanks!

Example
-------

This is an example showing you how to use the `ssq` function in our package to simulate a simple M/M/1 queue, passing in your own exponential interarrival function defined using our `vexp` variate generator, and then plot the number in the system across time with superimposed time-averaged statistics:

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

Installing
----------

Install the current version of `simEd` from CRAN using `install.packages("simEd")`.

Note that the `simEd` package depends on Josef Leydold's `rstream` package, a wrapper of Pierre L'Ecuyer's "mrg32k3a" random number generator, to provide independent streams of Uniform(0,1) random numbers. If the `rstream` package is not already installed, the previous step will install `rstream` automatically.

Details
-------

The goal of this package is to facilitate use of R for an introductory course in discrete-event simulation.

This package contains variate generators capable of independent streams (based on Josef Leydold's `rstream` package) and antithetic variates for two discrete and five continuous distributions:

-   discrete: `vbinom`, `vgeom`
-   continuous: `vexp`, `vgamma`, `vnorm`, `vunif`, `vweibull`

All of the variate generators use inversion, and are therefore monotone and synchronized.

The package contains functions to visualize variate generation for the same two discrete and five continuous distributions:

-   discrete: `ibinom`, `igeom`
-   continuous: `iexp`, `igamma`, `inorm`, `iunif`, `iweibull`

The package contains functions that are event-driven simulation implementations of a single-server queueing system and of a multiple-server queuing system:

-   single-server: `ssq`
-   multiple-server: `msq`

Both functions are extensible in allowing the user to provide custom arrival and service process functions.

The package contains three functions for computing time-persistent statistics:

-   time-average mean: `meanTPS`
-   time-average standard deviation: `sdTPS`
-   time-average quantiles: `quantileTPS`

The package also masks two functions from the stats package:

-   `set.seed`, which explicitly calls the stats version in addition to setting up seeds for the independent streams in the package;
-   `sample`, which provides capability to use independent streams and antithetic variates.

Finally, the package provides two queueing data sets to facilitate input modeling:

-   `queueTrace`, which contains 1000 (fabricated) arrival and service times for a single-server queueing system;
-   `tylersGrill`, which contains 1434 arrival times and 110 (sampled) service times corresponding to actual data collected during one business day at Tyler's Grill at the University of Richmond.
