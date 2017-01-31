#' @export
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