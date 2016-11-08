rgg=function(p,object,oldGeom,oldGeomIdx=1){
  if (is.null(object)) return(p)
  #objGeom=gsub("geom","",tolower(class(object$geom)[[1]]))
  #if(oldGeom!=objGeom) stop(paste0("geom of new object (",objGeom,") not equal to current object (",oldGeom,")"), call. = FALSE)
  p <- ggplot2:::plot_clone(p)
  p<-remove_geom(p = p,geom = oldGeom,idx = oldGeomIdx)+object
  ggplot2:::set_last_plot(p)
  p
}