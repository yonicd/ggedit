#' @title Plots theme elements in tile layout
#' @description S3 method to plot ggplot2 theme objects
#' @param x ggplot theme
#' @param ... objects to pass to plot (see details)
#' @details obj2: ggplot theme (optional as base to compare difference to x),
#' fnt:numeric font size of text in plot,
#' themePart:character vector that denotes the part of the theme to return NULL returns all. The options to choose are (line,rect,text,axis,legend,panel,plot,strip),
#' plotFrame:logical that nests the plot in a cheatsheet frame,
#' as.plotly:logical that invokes ggplotly to make the output interactive
#' @examples
#' \donttest{
#' plot(theme_bw(),fnt=10)
#' plot(theme_bw()%+replace%theme(axis.title = element_text(face='bold')),
#' fnt=12,themePart = c('axis','plot'))
#' plot(theme_bw(),obj2=theme_classic(),fnt=10,themePart='strip')
#' plot(theme_bw(),obj2=theme_classic(),plotFrame=FALSE)
#' plot(theme_bw(),obj2=theme_classic(),as.plotly = TRUE)
#' }
#' 
plot.theme=function(x,...){
  obj2=NULL
  fnt=11
  plotFrame=T
  as.plotly=F
  themePart=NULL
  
  l<-list(...)
  list2env(l,envir = environment())

  
  objName=paste0(deparse(substitute(x))," Red is active element")
  objList=themeFetchFull(x)
  objListDepth=sapply(objList,themeListDepth)
  
  objL=list(compare=x)
  
  if(!is.null(obj2)){
    objName=paste0('compare:',deparse(substitute(x)),' to ',
                   'base:',deparse(substitute(obj2)),
                    ' Blue updated element with new value, Red active element')
    
    objL=list(compare=x,base=obj2)
  } 
      
    objL=plyr::llply(objL,.fun = function(x){
      objList=themeFetchFull(x)
      objListDepth=sapply(objList,themeListDepth)      
      return(list(x=x,objList=objList,objListDepth=objListDepth))
    })
    
  
  objDF=plyr::ldply(objL,.fun = function(x){
    dfOut=bind_rows(
      x$objList[x$objListDepth==1]%>%plyr::ldply(.fun=function(x){
        out=x[-length(x)]%>%plyr::ldply(.id='element')
        out$call=x$call
        out
      },.id='Theme')%>%mutate(subTheme=NA)%>%mutate_each(funs(as.character)),
      
      x$objList[!x$objListDepth==1]%>%plyr::ldply(.fun=function(y) y%>%plyr::ldply(.fun=function(x){
        out=x[-length(x)]%>%plyr::ldply(.id='element')
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
  diffIdx=objDF%>%select(idTheme:value,subTheme)%>%reshape2::dcast(Theme+subTheme+element+name~idTheme,value.var='value')%>%filter(compare!=base)%>%left_join(objDF,by=c("Theme", "subTheme", "element", "name"))%>%
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
  rc=utils::tail(gsub('[a-z\\-]','',gt$layout$name[grepl('panel',gt$layout$name)]),1)
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
  if(as.plotly) pout=plotly::ggplotly(pout)
  return(pout)
}