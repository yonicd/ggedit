#' @export
#' @keywords internal
arg.value=function(item,obj.elems){
  item_class=obj.elems[[item]]$class[[1]]
  if(item_class=='data.frame'){
    if(item%in%c('colour','color','fill')){
      x=aesColour(item)
      y=obj.elems[[item]]$val[[1]]
      if(!grepl("[#]",y)) y=col2hcl(y)
      x$args$value=colourpicker:::closestColHex(y)[1]
      x=list(x=x)
    }else{
      x=aesSlide(item)
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
        x[['type']]=aesColourCont
        x$args=list(type=item)
        # x$args$selected='Blues'
        # x$args$choices=c(NA,'Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges', 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd')
        x=list(x=x)
      }else{
        stop("non colour aesthetics of numeric inputs are not currently supported in ggedit", call. = FALSE)
      }
    }
    if(item_class%in%c('character','factor')){
      x=lapply(obj.elems[[item]]$val[[1]],function(y){
        if(item%in%c('colour','color','fill')){
          z=aesColour(item)
          if(!grepl("[#]",y)) y=col2hcl(y)
          z$args$value=colourpicker:::closestColHex(y)[1]
        }else{
          z=aesSelect(item)
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
        x[[i]]$args$inputId=paste0(x[[i]]$args$inputId,i)
      }
    }
  }
  return(x)
}