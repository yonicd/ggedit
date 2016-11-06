remove_geom=function(p, geom,idx=1){
  layers=p$layers
  layer.type=lapply(p$layers,function(x) class(x$geom))
  a.rm=which(grepl(paste0("(?i)", geom), layer.type))
  if(length(a.rm)>=idx) a.rm=a.rm[idx]
  layers <- layers[-a.rm]
  p$layers <- layers
  p
}
