#' @title arg.valueNS
#' @description Creates list of arguments and values in gg layer for module.
#' @param item character type of gg layer
#' @param obj.elems elements in layer
#' @param session shiny session object
#' @return list
#' @keywords internal
arg.valueNS=function(item,obj.elems,session){
  ns<-session$ns
  item_class=obj.elems[[item]]$class[[1]]
  if(item_class=='data.frame'){
    if(item%in%c('colour','color','fill')){
      x=aesColourNS(item,session)
      y=obj.elems[[item]]$val[[1]]
      if(!grepl("[#]",y)) y=scales::col2hcl(y)
      x$args$value=closestColHex(y)[1]
      x=list(x=x)
    }else{
      x=aesSlideNS(item,session)
      x$args$value=obj.elems[[item]]$val[[1]]
      if(item=='alpha'){
        if(is.na(x$args[['value']])) x$args[['value']]=1
      }
      x=list(x=x)
    }
  }else{
    if(item_class=='numeric'){
      if(item%in%c('colour','color','fill')){
        x=vector('list',2)
        names(x)=c('type','args')
        x[['type']]=aesColourContNS
        x$args=list(type=item,session=session)
        x=list(x=x)
      }else{
        stop("non colour aesthetics of numeric inputs are not currently supported in ggedit", call. = FALSE)
      }
    }
    if(item_class%in%c('character','factor')){
      x=lapply(obj.elems[[item]]$val[[1]],function(y){
        if(item%in%c('colour','color','fill')){
          z=aesColourNS(item,session)
          if(!grepl("[#]",y)) y=scales::col2hcl(y)
          z$args$value=closestColHex(y)[1]
        }else{
          z=aesSelectNS(item,session)
          if(is.numeric(y)&!item%in%c('size','shape')){
            eval(parse(text=paste0('z$args$selected=c(',item,'_pal()(6)[',y,'])')))
          }else{
            z$args$selected=y
          }
          
          if(item=='shape') {
            z$args$choices=c(0:25)
          }
          if(item=='size') z$args$choices=c(0:10)
          if(!item%in%c('shape','size')) eval(parse(text=paste0('z$args$choices=c(0,',item,'_pal()(6))')))
        }
        
        return(z)
      })

      for(i in 1:length(x)){
        x[[i]]$args$label=paste0(x[[i]]$args$label,": Group ",i)
        x[[i]]$args$inputId=ns(paste0(x[[i]]$args$inputId,i))
      }
    }
  }
  
  return(x)
}