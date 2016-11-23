#' @title Plots theme elements in tile layout
#' @export
#' @param obj ggplot theme
#' @param fnt numeric font size of text in plot
#' @examples
#' plot(theme_bw(),fnt=10)

plot.theme=function(obj,fnt=11){
  objName=deparse(substitute(obj))
  objList=themeFetch(obj)
  
  objDF=rbind(objList[1:3]%>%ldply(.fun=function(x){
    out=x[-length(x)]%>%ldply(.id='element')
    out$call=x$call
    out
  },.id='Theme')%>%mutate(subTheme=NA),
  objList[-c(1:3)]%>%ldply(.fun=function(y) y%>%ldply(.fun=function(x){
    out=x[-length(x)]%>%ldply(.id='element')
    out$call=x$call
    out
  },.id='subTheme'),.id='Theme'))%>%
    mutate_each(funs(as.character))%>%mutate(subTheme=ifelse(is.na(subTheme),Theme,subTheme),
                                             Theme=factor(Theme,levels=names(objList)),
                                             value=ifelse(value%in%c("",NA),".",value))%>%
    do(.,cbind(.,elem_num=1:nrow(.)))%>%
    mutate(lbl=paste0(as.character(value),"^(",elem_num,")"))
  
  
  
  objDF%>%ggplot(aes(x=subTheme,y=element))+
    geom_tile(aes(fill=call),colour='black',alpha=0.25)+
    geom_text(aes(label=lbl),size=2,parse=T)+facet_wrap(~Theme,scales='free')+
    labs(title='Periodic Chart of GGplot Theme Elements',subtitle=objName)+
    scale_fill_discrete(name="Element Class")+
    theme(panel.background  = element_rect(fill='white',colour='black'),
          text=element_text(size=fnt),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(size=rel(0.7),angle=90),
          axis.text.y = element_text(size=rel(0.7)),
          strip.background = element_rect(fill='white',colour='black'),
          legend.position = 'top')
  
  
  
}