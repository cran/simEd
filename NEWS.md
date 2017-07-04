# simEd 1.0.2

* Updated ssq() and msq() help to include examples of passing trace data for
  arrival and service processes.

* Fixed a bug in each of the inorm and iunif functions in which a stray red
  horizontal line at F(x) = 0 was drawn for distributions with an x range
  having minimum less than 0.

# simEd 1.0.1

* Fixed a potential platform-dependent error in `quantileTPS`, which relied on 
  `cumsum` to produce an exact value for its last vector entry when summing.

* Includes minor updates to correct formatting issues in some of the pdf/pmf
  equations in help files for variate generator functions and inversion
  visualization functions.

