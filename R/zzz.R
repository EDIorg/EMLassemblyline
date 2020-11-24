# From: https://r-pkgs.org/r.html 
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.eal <- list(
    eal.env = .GlobalEnv
  )
  toset <- !(names(op.eal) %in% names(op))
  if(any(toset)) options(op.eal[toset])
  
  invisible()
}

.onUnload <- function(libname, pkgname) {
  options(eal.env = NULL)
  
  invisible()
}