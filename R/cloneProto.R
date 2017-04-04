cloneProto=function(l){
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
  
  g<-paste0('geom_',tolower(gsub('Geom','',class(x$geom)[1])),'()')
  g<-eval(parse(text=g))
    nm=names(x)
    nm=nm[!nm%in%c('geom','params','mapping')]
    
    part1<-paste0('geom_',tolower(gsub('Geom','',class(x$geom)[1])))
    
    part2<-paste0('mapping=aes(',paste0(lapply(names(x$mapping),
                                               function(item){
                                                 paste(item,x$mapping[[item]],sep="=")
                                               }
    ),
    collapse=","),
    ')')
    
    part3<-paste0(lapply(rev(names(unlist(x$params))),function(item) {
      cl=class(x$params[[item]])
      out=paste(item,x$params[[item]],sep="=") 
      if(cl=='character') out=paste(item,paste0("'",x$params[[item]],"'"),sep="=") 
      if(cl=='formula') out=paste0("formula=as.formula('",paste0(as.character(x$params[[item]])[-1],collapse="~"),"')")
      return(out)
    }),collapse=",")
    
    part4<-paste0(lapply(nm,function(y){
      if(is.logical(x[[y]])) out=paste(y,x[[y]],sep="=")
      if(is.character(x[[y]])) out=paste(y,paste0("'",x[[y]],"'"),sep="=")
      if(is.null(x[[y]])) out=paste(y,'NULL',sep="=")
      if(is.data.frame(x[[y]])) out=paste(y,"'[InputDataFrame]'",sep="=")
      return(out)
    }),collapse=',')
    
    nDF<-cbind(names(g$geom$default_aes),paste(g$geom$default_aes))
    nDF[nDF[,1]%in%c('colour','fill','color'),2]=paste0("'",scales::col2hcl(nDF[nDF[,1]%in%c('colour','fill','color'),2],alpha = NULL),"'")

    part5<-paste0(apply(nDF,1,function(x) paste0(x,collapse='=')),collapse=',')
    
    paste0(part1,'(',part2,',',part3,',',part4,',',part5,')')
    
    
}