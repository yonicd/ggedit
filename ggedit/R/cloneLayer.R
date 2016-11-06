cloneLayer=function(l){
  layer.names=c('mapping','data','geom','position',
                'stat','show.legend','inherit.aes',
                'aes_params','geom_params','stat_params')
  x=sapply(layer.names,function(y){
    b=l[[y]]
    
    if('waiver'%in%class(b)) b=NULL
    
    if(y=='geom') b=eval(parse(text=class(b)[1]))
    
    if(y%in%c('position','stat')) {
      b=gsub(y, "", tolower(class(b)[1]))
    }
    
    b
  }) 
  x$params=append(x$stat_params,x$geom_params)
  x$params=append(x$params,x$aes_params)
  x$params=x$params[!duplicated(names(x$params))]
  x$geom_params<-x$aes_params<-x$stat_params<-NULL
  do.call(layer,x)
}