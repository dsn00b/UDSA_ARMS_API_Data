.onLoad <- function(libname, pkgname) {
  metadata <<- refresh_metadata()
}

.onUnload <- function(libname, pkgname) {
  rm("metadata", pos = 1)
}