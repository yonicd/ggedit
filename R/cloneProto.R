cloneProto=function(l){
  
  geom_opts<-ggedit_opts$get('session_geoms')
  
  parent.layer<-proto_features(l)%>%
    dplyr::left_join(geom_opts%>%dplyr::filter_(~!grepl('^stat',fn)), 
                     by = c("position", "geom", "stat"))
  
  if(is.na(parent.layer$fn)) parent.layer$fn=paste0(tolower(strsplit(parent.layer$stat,'(?<=Stat)',perl = TRUE)[[1]]),collapse = '_')
  
  layer.names=c('mapping','data','geom','position',
                'stat','show.legend','inherit.aes',
                'aes_params','geom_params','stat_params')
  
  x=sapply(layer.names,function(y){

    b=l[[y]]

    if('waiver'%in%class(b)) b=NULL
    
    #if(y=='geom') b=eval(parse(text=class(b)[1]))
    
    if(y=='geom') b=eval(parse(text=parent.layer$geom))
    if(y=='position') b=gsub(y, "", tolower(class(b)[1]))
    if(y=='stat') b=eval(parse(text=parent.layer$stat))
    
    # if(y%in%c('position','stat')) {
    #   b=gsub(y, "", tolower(class(b)[1]))
    # }
    
    b
  }) 
  
  x$params=append(x$stat_params,x$geom_params)
  x$params=append(x$params,x$aes_params)
  x$params=x$params[!duplicated(names(x$params))]
  x$geom_params<-x$aes_params<-x$stat_params<-NULL
  
  fn=parent.layer$fn
  
  g<-paste0(fn,'()')
  g<-eval(parse(text=g))
    nm=names(x)
    
    nm=nm[!sapply(x,typeof)%in%c('environment','closure','list')]
    #nm=nm[!nm%in%c('geom','params','mapping')]
    
    geom_aes=list(geom   =fn,
                  mapping=paste0(names(x$mapping),sapply(x$mapping,build_map)),
                  params =paste0(names(x$params),sapply(x$params,build_map)),
                  layer  =paste0(rev(nm),sapply(x[rev(nm)],build_map))
          )

    nDF<-cbind(names(g$geom$default_aes),paste(g$geom$default_aes))
    nDF[grep('colour|fill|color',nDF[,1]),2]=paste0("'",scales::col2hcl(nDF[grep('colour|fill|color',nDF[,1]),2],alpha = NULL),"'")

    geom_aes$default<-paste0(apply(nDF,1,function(x) paste0(x,collapse='=')))
    
    #paste0(part1,'(',part2,',',part3,',',part4,',',part5,')')
    geom_aes
    
}