#' @title as.gglist
#' @description Creates structure of lists of ggplots.
#' @param p gg
#' @return gg
#' @export
#' @keywords internal
as.gglist=function(p){
  cl=c("ggedit","gglist", "gg","ggplot")
  if(inherits(p,'ggmatrix')) cl=c(cl,'ggmatrix')
  structure(p, class=cl) 
}

#' @export
#' @import ggplot2
#' @importFrom plyr llply
'+.gg'=function (e1, e2) {
  if(inherits(e1,'gglist')){
    as.gglist(plyr::llply(e1, function(t1){
      ggplot2:::add_ggplot(t1,e2)
    }))
  }else{
    if(inherits(e1,'ggmatrix')){
      add_ggmatrix(e1,e2)
    }else{
      if(inherits(e1,'theme')){
        ggplot2:::add_theme(e1,e2)  
      }else{
        ggplot2:::add_ggplot(e1,e2)   
      }
    }
  }
}