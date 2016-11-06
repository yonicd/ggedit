layersListObj=function(obj,lbl){
  a=lapply(obj,function(x){
    lapply(x$layers,function(y) y)
  })
  names(a)=lbl
  a
}
