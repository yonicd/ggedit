#' @title Clone ggplot2 scales from compiled ggplot objects
#' @description Clone ggplot2 scales from compiled ggplot objects returns not 
#' the original call but the nested call
#' @param p ggplot object
#' @param verbose boolean, if TRUE then returns script to eval(parse) if FALSE returns new compiled object Default: FALSE
#' @return ggplot scale or script (depends on verbose)
#' @examples 
#'  p<-pList$pointSmooth+scale_colour_continuous(low='red')
#'  p
#'  pList$pointSmooth+cloneScales(p)
#' @rdname cloneScales
#' @export 

cloneScales<-function(p,verbose=FALSE){
  sapply(p$scales$scales,function(obj){
    a<-as.character(obj$call)
    
    tempscale<-vector('list',length(names(formals(a[1]))))
    names(tempscale)<-names(formals(a[1]))
    
    tempscale.cl<-sapply(names(tempscale),function(idx) class(obj[[idx]]))
    
    skip.val<-grep('super',names(tempscale))
    
    for(idx in names(tempscale)[-skip.val]) tempscale[[idx]]<-obj[[idx]]
    
    tempscale$super<-formals(a[1])[['super']]
    
    scaleout<-do.call(a[1],tempscale)
    structure(scaleout, class=class(obj))
    
    if(verbose){
      scale_return<-paste0(capture.output(dput(scaleout$call)),collapse='\n') 
    }else{
      scale_return<-scaleout
    }
    return(scale_return)
  })}

