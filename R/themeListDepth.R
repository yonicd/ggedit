#' @title themeListDepth
#' @description Returns depth of nested list.
#' @param this list
#' @param thisdepth numeric of current depth in recursive call
#' @return numeric
#' @keywords internal
themeListDepth <- function(this,thisdepth=0){
  if(!is.list(this)){
    return(thisdepth)
  }else{
    return(max(unlist(lapply(this,themeListDepth,thisdepth=thisdepth+1))))    
  }
}