#' @title Is the object of class ggedit
#' @export
#' @description 
#' Is the object of class ggedit. Very basic for many functions in the package.
#' @param p an object
#' @return logical - is the object of class ggedit

is.ggedit<-function(p){
  inherits(p, "ggedit")
} 