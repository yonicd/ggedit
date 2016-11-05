fetch_aes_ggplotBuild=function(p,geom_list){
  train_map=function(p) {
    m=p$mapping
    m[names(m)[!names(m)%in%c('x','y')]]
  }
  
  layer_mapping=lapply(p$layers,function(x){
    train_map(x$mapping)
  })
  
  mapping_class=lapply(train_map(p),function(m) class(p$data[,as.character(m)]))
  
  p.Elems=lapply(p$layers,function(x) fetch_layer_aes(self=x$geom,params=x$aes_params))
  names(p.Elems)=geom_list
  gb=ggplot_build(p)
  data=gb$data
  cl=class_layer(p)
  for(i in 1:length(data)) data[[i]]=list(data=data[[i]],nm=names(p.Elems[[i]]),layer.map=layer_mapping[[i]],layer=geom_list(p)[i])
  
  l=lapply(data,function(x) {
    class.temp=sapply(names(x$data)[names(x$data)%in%x$nm],function(item) {
      class(as.data.frame(p$data)[,as.character(c(p$mapping[item][[1]],cl$var[cl$layer==x$layer&cl$aes==item]))])
    })
    l0=lapply(names(x$data)[names(x$data)%in%x$nm],
              function(item) {
                cl.layer=cl[cl$layer%in%c(x$layer,'plot'),]
                cl.item=cl.layer$class[cl.layer$aes==item]
                if(class.temp[item]=='factor') {
                  val=unique(x$data[,c(item,'group')])[item]
                }else{
                  val=unique(x$data[item])  
                }
                if(any(cl.layer$aes==item)) {
                  aes.var.nm=cl.layer$var[cl.layer$aes==item]
                  aes.var=p$data[aes.var.nm]
                  if(any(names(p$data)%in%names(gb$panel$layout))){
                    order.nms=c('PANEL',names(gb$panel$layout)[which(names(gb$panel$layout)%in%names(p$data))])
                    aes.var=p$data%>%
                      left_join(gb$panel$layout%>%select_(.dots=c(order.nms)),
                                by=order.nms[-1])%>%arrange_(order.nms)
                  }
                  if(nrow(x$data)!=nrow(aes.var)){
                    x$data[aes.var.nm]=factor(x$data$fill,labels=unique(aes.var[[aes.var.nm]]))
                    val.new=x$data%>%select_(item,aes.var.nm)%>%distinct()
                  }else{
                    val.new=data.frame(x$data,aes.var=as.character(aes.var[,aes.var.nm]),stringsAsFactors = F)%>%select_(item,'aes.var')%>%distinct()  
                  }
                  val=val.new[item]
                }
                list(val=val,class=class.temp[item])
              }
    )
    names(l0)=names(x$data)[names(x$data)%in%x$nm]
    return(l0)
  })
  names(l)=geom_list
  l
}