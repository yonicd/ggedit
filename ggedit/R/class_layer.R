#' @export
class_layer=function(p){

  plot_aes=layer_aes=NULL
  if(length(as.character(p$mapping))>0){
  plot_aes=as.character(p$mapping)
  aes.nm=names(p$mapping)[plot_aes!='NULL']
  plot_aes=plot_aes[plot_aes!='NULL']
  plot_aes=data.frame(var=unlist(plot_aes),stringsAsFactors = F)
  plot_aes$aes=aes.nm
  plot_cl=lapply(plot_aes$var,function(x){
    if('call'%in%class(p$mapping[[which(p$mapping==x)[1]]])){
      TEMP=p$data%>%mutate_(.NEWVAR=x)
      class(TEMP[,'.NEWVAR'])
    }else{
      class(p$data[,as.character(x)])  
    }
  })
    
  chkIdx=which(unlist(lapply(plot_cl,length))!=1)
  for(i in chkIdx){
    if('data.frame'%in%plot_cl[[i]]){
      plot_cl[[i]]='data.frame'
    }else{
        break(paste0('classes:c(',paste0(plot_cl[[i]],collapse=','),') for plot variable:', plot_aes$var[i],'. Please choose one and rerun'))
      }
  }
  
  plot_aes$class=unlist(plot_cl)
  plot_aes$layer='plot'}
  
  layer_aes=lapply(p$layers,function(x) data.frame(var=as.character(x$mapping),aes=names(x$mapping),stringsAsFactors = F))
  layer_data=lapply(p$layers,function(x) x$data)
  names(layer_aes)=names(layer_data)=geom_list(p)
  layer_aes=ldply(layer_aes,.id = 'layer')

  layer_aes=ddply(layer_aes,.(var,aes),.fun=function(df){
    
    if(class(layer_data[[df$layer]])=='waiver'){
        pData=p$data
      }else{
        pData=layer_data[df$layer][[1]]
      }
    
    if(!df$var%in%names(pData)){
      TEMP=p$data%>%mutate_(.NEWVAR=df$var)
      df$class=class(TEMP[,'.NEWVAR'])
    }else{
      df$class=class(pData[,df$var])
    }
    
    df
  })
  
  
  layer_bind=rbind(plot_aes,layer_aes)
  
  layer_bind=ddply(layer_bind,.(var,aes),.fun=function(df){
    if(class(layer_data[[df$layer]])%in%c('waiver','NULL')){
      pData=p$data
    }else{
      pData=layer_data[df$layer][[1]]
    }
    if(!df$var%in%names(pData)){
      TEMP=p$data%>%mutate_(.NEWVAR=df$var)
      df$level.num=length(unique(TEMP[,'.NEWVAR']))
    }else{
      df$level.num=length(unique(pData[,df$var]))
    }
    
    df
  })
  
  return(layer_bind)
}