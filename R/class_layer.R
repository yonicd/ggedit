#' @title class_layer
#' @description Determines the class of the arguments in a gg layer.
#' @param p ggplot
#' @return data.frame
#' @keywords internal
#' @import ggplot2
#' @import dplyr
#' @importFrom  plyr ddply ldply
#' 
class_layer=function(p){

  if('tbl'%in%class(p$data)) p$data=data.frame(p$data)
  
  plot_aes=layer_aes=NULL
  if(length(as.character(p$mapping))>0){
  plot_aes=as.character(p$mapping)
  aes.nm=names(p$mapping)[plot_aes!='NULL']
  plot_aes=plot_aes[plot_aes!='NULL']
  plot_aes=data.frame(VAR=unlist(plot_aes),stringsAsFactors = F)
  plot_aes$aes=aes.nm
  plot_cl=lapply(plot_aes$VAR,function(x){
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
        break(paste0('classes:c(',paste0(plot_cl[[i]],collapse=','),') for plot variable:', plot_aes$VAR[i],'. Please choose one and rerun'))
      }
  }
  
  plot_aes$class=unlist(plot_cl)
  plot_aes$layer='plot'}
  
  layer_aes=lapply(p$layers,function(x) data.frame(VAR=as.character(x$mapping),aes=names(x$mapping),stringsAsFactors = F))
  layer_data=lapply(p$layers,function(x){
   dOut=x$data
   if('tbl'%in%class(dOut)) dOut=data.frame(dOut)
   dOut
  })
  names(layer_aes)=names(layer_data)=geom_list(p)
  layer_aes=plyr::ldply(layer_aes,.id = 'layer')

  layer_aes=plyr::ddply(layer_aes,.variables = c('VAR','aes'),.fun=function(df){
    
    if('waiver'%in%class(layer_data[[df$layer]])){
        pData=p$data
      }else{
        pData=layer_data[df$layer][[1]]
      }
    
    if(!df$VAR%in%names(pData)){
      TEMP=p$data%>%mutate_(.NEWVAR=df$VAR)
      df$class=class(TEMP[,'.NEWVAR'])
    }else{
      df$class=class(pData[,df$VAR])
    }
    
    df
  })
  
  
  layer_bind=rbind(plot_aes,layer_aes)
  
  layer_bind=plyr::ddply(layer_bind,.variables = c('VAR','aes'),.fun=function(df){
    if(class(layer_data[[df$layer]])%in%c('waiver','NULL')){
      pData=p$data
    }else{
      pData=layer_data[df$layer][[1]]
    }
    if(!df$VAR%in%names(pData)){
      TEMP=p$data%>%mutate_(.NEWVAR=df$VAR)
      df$level.num=length(unique(TEMP[,'.NEWVAR']))
    }else{
      df$level.num=length(unique(pData[,df$VAR]))
    }
    
    df
  })
  
  return(layer_bind)
}