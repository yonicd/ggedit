#' @export
themePanelNS=function(obj.in,obj.name,session){
  ns<-session$ns
  lapply(names(obj.in) ,FUN = function(item){
    do.call(what = tabPanel,
            args = list(title=item,
                        fluidRow(lapply(head(names(obj.in[[item]]),-1),
                                        FUN=function(subitem){
                                          x=obj.in[[item]][[subitem]]
                                          txt.in=unique(c(obj.name,item,subitem))
                                          if(obj.name=="legend"&item%in%c("position","justification")) txt.in=c(obj.name,item,subitem)
                                          obj=column(width=3,textInput(inputId = ns(paste0(c("pop",txt.in),collapse="")),
                                                                       label = x['name'],
                                                                       value = x['value'])
                                          )
                                          return(obj)
                                        })),
                        HTML(paste0(
                          paste(paste0('<b>',names(themeTips[[obj.in[[item]]$call]]),'</b>'),
                                unlist(themeTips[[obj.in[[item]]$call]]),
                                sep=': '),
                          sep="<br/>"))
            )
    )
  }
  )
}