remove_geom=function(p, geom,idx){
  layers=p$layers
  layer.type=lapply(p$layers,function(x) class(x$geom))
  a.rm=which(grepl(paste0("(?i)", geom), layer.type))
  
  if(length(a.rm)==0) stop(paste0("There are less no ",geom," layers available in the plot to remove"), call. = FALSE)
  
  if(idx>length(a.rm)) stop(paste0("There are less than ",idx," ",geom," layers available in the plot to remove"), call. = FALSE)
  
  if(length(a.rm)>=idx) a.rm=a.rm[idx]
  layers <- layers[-a.rm]
  p$layers <- layers
  p
}
