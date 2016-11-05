layersList=function(obj){
  if(is.ggplot(obj)) obj=list(obj)
  rmNullObs(lapply(obj,layersListFull))
}
