.onLoad <- function(libname, pkgname) {
  if(!'shinyBS'%in%loadedNamespaces()) attachNamespace('shinyBS')
}

.onAttach <- function(libname, pkgname) {
  if(!'shinyBS'%in%loadedNamespaces()) attachNamespace('shinyBS')
}

setHook(packageEvent("shiny", "onLoad"),
        function(...) if(!'shinyBS'%in%loadedNamespaces()) attachNamespace('shinyBS'))
