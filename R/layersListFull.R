#' @title layersListFull
#' @description Runs the ggplot_build function on the input and converts 
#' the output data objects into a nested list with the unique values of 
#' each of the aesthetic columns. Includes all objects used for theme plots
#' @param obj ggplot2 plot object or list of plot objects
#' @return list of aesthetics and their values for each layer in a plot
#' @keywords internal
layersListFull=function(obj) {
  x=fetch_aes_ggplotBuild(obj,geom_list = geom_list(obj))
  y=lapply(x,function(x1) {lapply(x1,function(y) {
    y1=y$val
    names(y1)=NULL
    if(!all(is.na(y1))) return(y1)
  })
  })
  return(y)
}