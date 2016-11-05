geom_list=function(p) {
  g=gsub('Geom','',unlist(lapply(p$layers,function(x) class(x$geom)[1])))
  g.list=sapply(unique(g),function(x) paste0(g[g==x],seq(1,table(g)[[x]])))
  unlist(g.list)
}