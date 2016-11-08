`%-%`=function(e1,e2){
  e2name=deparse(substitute(e2))
  e2obj=unlist(lapply(strsplit(deparse(substitute(c(l,'point',1))),','),function(x) gsub('[c() ]','',x)))
  l=eval(parse(text=e2obj[1]))
  oldGeom=e2obj[2]
  oldGeomIdx=as.numeric(e2obj[3])
  if (is.ggplot(e1)) e1=rgg(e1,l,oldGeom,oldGeomIdx)
  e1
}

NULL