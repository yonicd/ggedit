#' @title Plots theme elements in tile layout
#' @export
#' @param obj ggplot theme
#' @param obj2 ggplot theme (optional as base to compare difference to obj)
#' @param fnt numeric font size of text in plot
#' @param themePart character vector that denotes the part of the theme to return NULL returns all. The options to choose are (line,rect,text,axis,legend,panel,plot,strip)
#' @param plotFrame logical that nests the plot in a cheatsheet frame
#' @param plotly logical that invokes ggplotly to make the output interactive
#' @examples
#' plot(theme_bw(),fnt=10)
#' plot(theme_bw()%+replace%theme(axis.title = element_text(face='bold')),fnt=12,themePart = c('axis','plot'))
#' plot(theme_bw(),theme_classic(),fnt=10,themePart='strip')
#' plot(theme_bw(),theme_classic(),plotFrame=F)
#' plot(theme_bw(),theme_classic(),plotly = T)

plot.theme=function(obj,obj2=NULL,fnt=11,themePart=NULL,plotFrame=T,as.plotly=F){

  objName=paste0(deparse(substitute(obj))," Red is active element")
  objList=ggedit:::themeFetchFull(obj)
  objListDepth=sapply(objList,ggedit:::themeListDepth)
  
  objL=list(compare=obj)
  
  if(!is.null(obj2)){
    objName=paste0('compare:',deparse(substitute(obj)),' to ',
                   'base:',deparse(substitute(obj2)),
                    ' Blue updated element with new value, Red active element')
    
    objL=list(compare=obj,base=obj2)
  } 
      
    objL=llply(objL,.fun = function(obj){
      objList=ggedit:::themeFetchFull(obj)
      objListDepth=sapply(objList,ggedit:::themeListDepth)      
      return(list(obj=obj,objList=objList,objListDepth=objListDepth))
    })
    
  
  objDF=ldply(objL,.fun = function(obj){
    dfOut=bind_rows(
      obj$objList[obj$objListDepth==1]%>%ldply(.fun=function(x){
        out=x[-length(x)]%>%ldply(.id='element')
        out$call=x$call
        out
      },.id='Theme')%>%mutate(subTheme=NA)%>%mutate_each(funs(as.character)),
      
      obj$objList[!obj$objListDepth==1]%>%ldply(.fun=function(y) y%>%ldply(.fun=function(x){
        out=x[-length(x)]%>%ldply(.id='element')
        out$call=x$call
        out
      },.id='subTheme'),.id='Theme')%>%mutate_each(funs(as.character))
    )
      dfOut%>%
        mutate(subTheme=ifelse(is.na(subTheme),"",subTheme),
               Theme=factor(Theme,levels=names(objList)),
               value=ifelse(value%in%c("",NA),".",value)
        )%>%
        do(.,cbind(.,elem_num=1:nrow(.)))%>%
        mutate(class=ifelse(call=='unit','unit',class),
               value=ifelse(grepl('#',value),gsub("#","'#'*",value),value),
               value=ifelse(grepl('TRUE|FALSE|[=]',value),paste0("'",value,"'"),value),
               lbl=paste0(as.character(value),"[",class,"]^(",elem_num,")"),
               call=ifelse(call%in%c('character','unit',subTheme),'',call),
               element=ifelse(element==subTheme,'',element),
               colLbl=ifelse(value%in%c('.',''),'a','b')
        )
    
  },.id = 'idTheme')
  
  if(!is.null(obj2)){
  diffIdx=objDF%>%select(idTheme:value,subTheme)%>%dcast(Theme+subTheme+element+name~idTheme,value.var='value')%>%filter(compare!=base)%>%left_join(objDF,by=c("Theme", "subTheme", "element", "name"))%>%
    mutate(
    lbl=paste0(as.character(base),"[",class,"]",as.character(compare),"[",class,"]^(",elem_num,")"),
    colLbl='c'
  )%>%filter(idTheme=='compare')%>%select(elem_num,colLbl,lbl)}
    
  objDF=objDF%>%filter(idTheme=='compare')
  
  if(!is.null(obj2)) objDF$colLbl[which(objDF$elem_num%in%diffIdx$elem_num)]=diffIdx$colLbl
  
  lblSz=1
  xaxis.angle=90
  
  if(!is.null(themePart)) {
    objDF=objDF%>%filter(Theme%in%themePart)
    lblSz=3
    xaxis.angle=0
  }
  
  N<-objDF %>% select(Theme,call,subTheme)%>%unique%>%
    count(Theme,call)%>%
    ungroup%>%arrange(n)%>%
    mutate(ThemeCall=paste(Theme,call,sep="\n"))  
  
  objDF=objDF%>%mutate(ThemeCall=paste(Theme,call,sep="\n"))
  objDF$ThemeCall=factor(objDF$ThemeCall,levels=N$ThemeCall)
  colVals=c("grey","red",'blue')[1:length(unique(objDF$colLbl))]
  
  if(as.plotly){
    lblVar='value'
  }else{
    lblVar='lbl'
  }
  
  p=objDF%>%ggplot(aes(x=subTheme,y=element))+
    geom_tile(aes(fill=colLbl),colour='black',alpha=0.25)+
    geom_text(aes_string(label=lblVar),size=fnt/5,parse=T)+
    facet_wrap(~ThemeCall,scales='free',shrink=T,dir = 'v')+
    scale_fill_manual(values=colVals)+
    theme(panel.background  = element_rect(fill='white'),
          text=element_text(size=fnt),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(size=rel(0.7)),
          axis.text.y = element_text(size=rel(0.7)),
          strip.background = element_rect(fill='white',colour='black'),
          legend.position = 'none')
  
  gt = ggplotGrob(p)
  numCol=sum(as.numeric(gt$widths)==1)^2
  newN=N$n
  rc=tail(gsub('[a-z\\-]','',gt$layout$name[grepl('panel',gt$layout$name)]),1)
  r1=as.numeric(substr(rc,1,1))
  c1=as.numeric(substr(rc,2,2))

  if(nrow(N)<r1*c1) newN=c(N$n,rep(0,r1*c1-nrow(N)))
  gtl=apply(matrix(newN,ncol=r1,nrow=c1),2,max)
  gtln=length(gtl)
  gt$widths[seq(4,4*gtln,4)]<-unit(gtl,'null')

  pEx=data.frame(x=1,y=1,f='ThemeType\nElementClass')%>%
    ggplot(aes(x,y))+geom_tile(fill='white')+
    #geom_text(aes(label="value[class]^(index)"),parse=T)+
    facet_wrap(~f)+
    labs(x="x axis: element",
         y='y axis: ElementClassArgument',
         title='Construct A Theme',
         subtitle='Theme(ThemeType.element=ElementClass(ElementClassArgument=value))\n No ElementClass: Theme(ThemeType.element=value)',
         caption=objName
    )+
    theme(panel.background  = element_rect(fill='white',colour='black'),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title.y = element_text(vjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.title = element_text(hjust = 0.5),
          strip.background = element_rect(fill='white',colour='black'),
          legend.position = 'none',
          plot.background = element_rect(colour='black')
    )

  pout=pEx+annotation_custom(grob=gt)
  
  if(!plotFrame||as.plotly) pout=p
  if(as.plotly) pout=ggplotly(pout)
  return(pout)
}