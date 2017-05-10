simEd_env <- new.env(parent = emptyenv())
simEd_env$simEd_max_streams <- 25
simEd_env$simEd_streams <- NULL

.onLoad <- function(libname, pkgname) 
{
  # create default streams with internally generated initial seed
  simEd_env$simEd_streams <- lapply( 
    rep("rstream.mrg32k3a", simEd_env$simEd_max_streams), methods::new)

  invisible() # return val can be assigned, but doesn't print if not assigned
}

.onUnload <- function(libpath)
{
}
