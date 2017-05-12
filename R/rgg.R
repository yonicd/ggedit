#' @title Remove and replace ggplot2 layers.
#' @export
#' @description 
#' Removes specified layers from a ggplot object and gives 
#' the option to replace them with a new layer. This layer can be either 
#' a geom object created from regular ggplot functions or an output from 
#' the ggedit gadget. 
#' In the latter case the layers are found in the updatedLayers object in 
#' the ggedit output.
#' @param p ggplot2 plot object
#' @param oldGeom character string of the name of the layer to remove
#' @param oldGeomIdx numeric of which index of OldGeom to remove (default is 1)
#' @param newLayer ggplot layer or list of layers
#' @examples
#' p=ggplot2::ggplot(iris,ggplot2::aes(x =Sepal.Length,y=Sepal.Width))
#' p=p+ggplot2::geom_point(ggplot2::aes(colour=Species))+ggplot2::geom_line()
#' p
#' p%>%rgg('point',1)
#' \donttest{
#' x=ggedit(p)
#' pnew=p%>%rgg('point',1,x$updatedLayers[[1]])
#' pnew
#' }
#' @import ggplot2
rgg=function(p,oldGeom,oldGeomIdx=1,newLayer=NULL){
  if (is.null(oldGeom)) return(p)
  p <- plot_clone(p)
  p<-remove_geom(p = p,geom = oldGeom,idx = oldGeomIdx)+newLayer
  set_last_plot(p)
  p
}