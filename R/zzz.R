simEd_env <- new.env(parent = emptyenv())
simEd_env$simEd_max_streams <- 25
simEd_env$simEd_streams <- NULL

 # Might want to either switch to hard-coding or renaming w/ longer  name
 # to avoid possible conflicts
sym <- lapply(list(
  alpha    = "\u03B1",
  arrow    = "\u2192",
  beta     = "\u03B2",
  chi      = "\u03C7",
  Delta    = "\u0394",
  delta    = "\u03B4",
  infinity = "\u221E",
  Gamma    = "\u0393",
  gamma    = "\u03B3",
  lambda   = "\u03BB",
  mu       = "\u03BC",
  nu       = "\u03BD",
  sigma    = "\u03C3",
  theta    = "\u03B8",
  dots     = "\u2026",
  bullet   = "\u2022",
  alert    = "<!>" 
# ), Encoding)
), function(x) x)

.onLoad <- function(libname, pkgname)
{
  Sys.setlocale("LC_ALL")
  pdf.options(encoding='ISOLatin2.enc')
  # create default streams with internally generated initial seed
  simEd_env$simEd_streams <- lapply(
    rep("rstream.mrg32k3a", simEd_env$simEd_max_streams), methods::new)

  invisible() # return val can be assigned, but doesn't print if not assigned
}

.onUnload <- function(libpath)
{
}
