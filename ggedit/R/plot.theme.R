#' @title Plots theme elements in tile layout
#' @export
#' @param obj ggplot theme
#' @param fnt numeric font size of text in plot
#' @param themePart character vector that denotes the part of the theme to return NULL returns all. 
#' The options to choose are (line,rect,text,axis,legend,panel,plot,strip)
#' @examples
#' plot(theme_bw(),fnt=10)
#' plot(theme_bw()%+replace%theme(axis.title = element_text(face='bold')),fnt=12,themePart = c('axis','plot'))

plot.theme=function(obj,fnt=11,themePart=NULL){
  objName=deparse(substitute(obj))
  objList=themeFetch(obj)
  
  objListDepth=sapply(objList,themeListDepth)
  
  
  
  objDF=rbind(objList[objListDepth==1]%>%ldply(.fun=function(x){
    out=x[-length(x)]%>%ldply(.id='element')
    out$call=x$call
    out
  },.id='Theme')%>%mutate(subTheme=NA),
  objList[!objListDepth==1]%>%ldply(.fun=function(y) y%>%ldply(.fun=function(x){
    out=x[-length(x)]%>%ldply(.id='element')
    out$call=x$call
    out
  },.id='subTheme'),.id='Theme'))%>%
    mutate_each(funs(as.character))%>%mutate(subTheme=ifelse(is.na(subTheme),"",subTheme),
                                             Theme=factor(Theme,levels=names(objList)),
                                             value=ifelse(value%in%c("",NA),".",value))%>%
    do(.,cbind(.,elem_num=1:nrow(.)))%>%
    mutate(class=ifelse(call=='unit','unit',class),
           lbl=paste0(as.character(value),"[",class,"]^(",elem_num,")"),
           call=ifelse(call%in%c('character','unit',subTheme),'',call),
           element=ifelse(element==subTheme,'',element)
    )
  
  lblSz=1
  xaxis.angle=90
  
  if(!is.null(themePart)) {
    objDF=objDF%>%filter(Theme%in%themePart)
    lblSz=3
    xaxis.angle=0
  }
  
  objDF%>%ggplot(aes(x=subTheme,y=element))+
    geom_tile(aes(fill=Theme),colour='black',alpha=0.25)+
    geom_text(aes(label=lbl),size=lblSz,parse=T)+facet_wrap(Theme~call,scales='free')+
    labs(title='Periodic Chart of GGplot Theme Elements',
         subtitle=paste0("theme call: ",objName))+
    scale_fill_discrete(name="Element Class")+
    theme(panel.background  = element_rect(fill='white'),
          text=element_text(size=fnt),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(size=rel(0.7),angle=xaxis.angle),
          axis.text.y = element_text(size=rel(0.7)),
          strip.background = element_rect(fill='white',colour='black'),
          legend.position = 'none')
}