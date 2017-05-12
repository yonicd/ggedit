#' @title layersList
#' @export
#' @description Runs the ggplot_build function on the input and converts
#'  the output data objects into a nested list with the unique values of 
#'  each of the aesthetic columns.
#' @param obj ggplot2 plot object or list of plot objects
#' @return list of aesthetics and their values for each layer in a plot
#' @examples
#' p=ggplot2::ggplot(iris,ggplot2::aes(x=Sepal.Length,y=Sepal.Width))
#' p=p+ggplot2::geom_point(ggplot2::aes(colour=Species))+ggplot2::geom_line()
#' p
#' p.list=layersList(p)
#' p.list
#' 
layersList=function(obj){
  if(is.ggplot(obj)) obj=list(obj)
  rmNullObs(lapply(obj,layersListFull))
}