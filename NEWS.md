# simEd 2.0.1

* Updated author email addresses to current affiliations.
* Removed `show` parameter from `msq` and `ssq` because of inconsistency with
  meaning/values of `show` parameter in other functions.  Ability to select
  components to display is still available through other existing parameters.
* Added `restorePar` parameter to all i* functions to allow the user to request
  that prior par values not be restored on function exit, permitting multiple 
  visualizations per plot (e.g., 3x3) and/or drawing overlays on visualization.

# simEd 2.0.0

* Added `ssqvis` to illustrate and animate the details of an event-driven
  implementation of a single-server queueing system.  Includes depictions
  of the calendar, inversion for variate generation, system clock and
  timeline, the state of the queueing system, skyline functions, and
  time-persistent and based-on-observation statistics.
* Updated `ssq` and `msq` to include animation capability, illustrating
  current state of the system and skyline functions.
* Added `lehmer` to illustrate and animate the details of a multiplicative
  linear congruential random number generator.
* Added `accrej` to illustrate the technique of acceptance rejection for
  variate generation in situations when inversion is not possible (e.g.,
  for the Beta distribution).
* Added `thinning` to illustrate the technique of thinning for generating
  a non-homogeneous Poisson process.
* Updated `v*` functions to allow returning in list form, primarily for
  use in `ssqvis`.
* Updated `i*` functions to improve step-by-step animation, and to plot
  quantiles only in the inversion process when many variates are generated,
  thereby reducing visual noise.

# simEd 1.0.3

* Added Monte Carlo simulation functions `galileo` and `craps` to simulate,
  respectively, Galileo's dice and the game of craps.
* Includes minor updates to all inversion visualization functions (`ibinom`,
  `iexp`, `igeom`, `igamma`, `inorm`, `iunif`, and `iweibull`), allowing for
  display or suppression of plot title, and to respect by default any existing
  device layout but with ability to override via parameter.
* Includes minor corrections and addenda to pdf/pmf equations in help files.

# simEd 1.0.2

* Updated `ssq` and `msq` help to include examples of passing trace data for
  arrival and service processes.

* Fixed a bug in each of the `inorm` and `iunif` functions in which a stray red
  horizontal line at F(x) = 0 was drawn for distributions with an x range
  having minimum less than 0.

# simEd 1.0.1

* Fixed a potential platform-dependent error in `quantileTPS`, which relied on 
  `cumsum` to produce an exact value for its last vector entry when summing.

* Includes minor updates to correct formatting issues in some of the pdf/pmf
  equations in help files for variate generator functions and inversion
  visualization functions.

