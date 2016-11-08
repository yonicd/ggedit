rgg=function(p,oldGeom,oldGeomIdx=1,object=NULL){
  if (is.null(oldGeom)) return(p)
  p <- ggplot2:::plot_clone(p)
  p<-remove_geom(p = p,geom = oldGeom,idx = oldGeomIdx)+object
  ggplot2:::set_last_plot(p)
  p
}