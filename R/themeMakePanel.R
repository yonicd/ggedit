#' @title themeMakePanel
#' @description BS modal production for the gg theme.
#' @param x list from themeFetch
#' @return UI
#' @import shiny
#' @keywords internal
themeMakePanel=function(x,y=NULL){
  if(any(names(x[[1]])=='call')){
    if(is.null(y)) y=names(x)
    out=themePanel(x,y)
  }else{
    a=lapply(names(x[[1]]),FUN = function(y) {
      themeMakePanel(x=x[[1]][y],y=names(x))
    })
    out=shiny::tabPanel(title=names(x),do.call(what = shiny::tabsetPanel,args = unlist(a,recursive = F)))
  }
  return(out)
}