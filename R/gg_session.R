#' @title retreive all functions that create ggroto layers or stats
#' @description retreive all functions that create ggroto layers or stats
#' @param gg_pkg character, package names to search in if NULL an internal 
#' search will look in loaded packages, Default: NULL
#' @return data.frame
#' @export 
#' @examples 
#' gg_session('ggplot2')
#'
#' @importFrom plyr ldply
#' @importFrom reshape2 dcast
#' @importFrom stringr str_extract_all
#' @importFrom tools package_dependencies
gg_session=function(gg_pkg=NULL){
  if(is.null(gg_pkg)){
    gg_pkg=names(which(sapply(tools::package_dependencies(names(sessionInfo()[['otherPkgs']])),function(x) any(grepl('ggplot2',x))))) 
    gg_pkg=c('ggplot2',gg_pkg[!grepl('ggedit',gg_pkg)])
  }
  fn<-unlist(sapply(gg_pkg,function(x){
    y<-ls(sprintf('package:%s',x))
    y<-y[!grepl("[^[:alnum:][:space:]_]",y)]
    sprintf('%s::%s',x,y)
  },USE.NAMES = FALSE))
  
  x<-sapply(fn,function(x){
    gsub('[,]','',unlist(stringr::str_extract_all(capture.output(eval(parse(text=x))),'geom = (.*?),|stat = (.*?),|position = (.*?),')))
  })
  
  x1<-x[lapply(x,length)>0]
  x1<-x1[sapply(x1,function(x) any(grepl('stat',x)))]
  
  y<-lapply(x1,function(y) y[!y%in%c('geom = geom','stat = stat','position = position',
                                     'position = title.position',
                                     'position = label.position','position = NULL',
                                     'stat = list(NULL)','position = list(NULL)')])
  
  out<-plyr::ldply(y[sapply(y,length)>0],.fun=function(x){
    data.frame(do.call('rbind',strsplit(x,' = ')),stringsAsFactors = FALSE)
  } ,.id='fn')%>%reshape2::dcast(fn~X1,value.var='X2')%>%
    filter(.,complete.cases(.))%>%mutate_all(as.character)
  
  out[,c('position','stat','geom')]=sapply(c('position','stat','geom'),function(x){
    y=which(grepl('"',out[[x]]))
    z<-sapply(strsplit(gsub('"','',out[[x]][y]),'_'),function(a){
      paste0(gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",c(x,a), perl=TRUE),collapse = '')
    })
    out[[x]][y]=z
    out[[x]]
  })
  
  out$pkg=gsub(':(.*?)$','',out$fn)
  out$fn=gsub('^(.*?):','',out$fn)  
  out
}