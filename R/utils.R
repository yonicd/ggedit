#' @inherit magrittr::`%>%`
#' @export
`%>%`=magrittr::'%>%'

build_map=function(y){
  item=names(y)
  out=paste(item,y,sep="=") 
  if(is.character(y)) out=paste(item,paste0("'",y,"'"),sep="=") 
  if(inherits(y,'formula')) out=paste0("formula=as.formula('",paste0(as.character(y)[-1],collapse="~"),"')")
  if(is.null(y)) out=paste(item,'NULL',sep="=")
  if(inherits(y,c('function','call'))) out=paste0(item,'=',paste(utils::capture.output(dput(y)),collapse='\n'))
  if(inherits(y,c('data.frame'))) out=paste0('=',paste(utils::capture.output(dput(y)),collapse='\n'))
  return(out)
  }
