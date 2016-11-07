themeFetch=function(a=theme_get()){
  obj.names=names(a)[which(lapply(a,length)>0)]
  obj.names=obj.names[!grepl('margin|ticks.length|ontop|switch|key.size',obj.names)]
  obj.names.split=regmatches(obj.names, regexpr("[.]", obj.names), invert = TRUE)
  panel.names=unique(unlist(lapply(regmatches(obj.names, regexpr("[.]", obj.names), invert = TRUE),'[',1)))
  vout=vector('list',length(panel.names))
  names(vout)=panel.names
  for(i in 1:length(obj.names)){
    obj.class=lapply(a[[obj.names[i]]],function(x) class(x)[1])
    obj.val=lapply(a[[obj.names[i]]],'[',1)
    obj.name=names(a[[obj.names[i]]])
    if(is.null(obj.name)) obj.name=obj.names.split[[i]][2]
    obj.val[unlist(lapply(obj.class,function(x) x=="NULL"))]=""
    idx=which(!obj.name%in%c('margin','debug','inherit.blank','arrow'))
    obj=mapply(c, name=obj.name[idx],value=obj.val[idx], class=obj.class[idx],SIMPLIFY=FALSE)
    
    obj$call=class(a[[obj.names[i]]])[1]
    if(obj.names[i]%in%c('legend.position','legend.justification')) obj$call=gsub('legend.','',obj.names[i])
    if(length(obj.names.split[[i]])==1){
      vout[[obj.names.split[[i]][1]]]=obj
    }else{
      vout[[obj.names.split[[i]][1]]][[obj.names.split[[i]][2]]]=obj
    }
    
  }
  return(vout)
}