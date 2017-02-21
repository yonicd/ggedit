#' @export
#' @keywords internal
themeListDepth <- function(this,thisdepth=0){
  if(!is.list(this)){
    return(thisdepth)
  }else{
    return(max(unlist(lapply(this,themeListDepth,thisdepth=thisdepth+1))))    
  }
}