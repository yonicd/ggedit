#' @export
#' @keywords internal
themeMakePanelNS=function(x,y=NULL,session){
  if(any(names(x[[1]])=='call')){
    if(is.null(y)) y=names(x)
    out=themePanelNS(x,y,session)
  }else{
    a=lapply(names(x[[1]]),FUN = function(y) {
      themeMakePanelNS(x=x[[1]][y],y=names(x),session=session)
    })
    out=tabPanel(title=names(x),do.call(what = tabsetPanel,args = unlist(a,recursive = F)))
  }
  return(out)
}