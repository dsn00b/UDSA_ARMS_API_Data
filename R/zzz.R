.onLoad <- function(libname, pkgname) {
  usda.arms.api.env <<- new.env()
  usda.arms.api.env$metadata <<- refresh_metadata()
}

.onUnload <- function(libname, pkgname) {
  rm("usda.arms.api.env", pos = 1)
}