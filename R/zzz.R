.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath("sbs", system.file("www", package="shinyBS"))
}

.onAttach <- function(libname, pkgname) {
  shiny::addResourcePath("sbs", system.file("www", package="shinyBS"))
}
