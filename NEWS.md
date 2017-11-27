# simEd 1.0.3

* Added Monte Carlo simulation functions `galileo` and `craps` to simulate,
  respectively, Galileo's dice and the game of craps.
* Includes minor updates to all inversion visualization functions (`ibinom`,
  `iexp`, `igeom`, `igamma`, `inorm`, `iunif`, and `iweibull`), allowing for
  display or supression of plot title, and to respect by default any existing
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

