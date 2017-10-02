.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath("sbs", system.file("www", package="shinyBS"))
}

.onAttach <- function(libname, pkgname) {
  shiny::addResourcePath("sbs", system.file("www", package="shinyBS"))
}

setHook(packageEvent("shiny", "onLoad"),
        function(...) shiny::addResourcePath("sbs", system.file("www", package="shinyBS"))
        )
