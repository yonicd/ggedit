#' @title themePanelNS
#' @description UI production for the gg theme, for modal.
#' @param obj.in theme object in list form
#' @param obj.name name of theme object
#' @param session shiny session object
#' @return UI
#' @import shiny
#' @keywords internal
themePanelNS=function(obj.in,obj.name,session){
  ns<-session$ns
  
  themeTips <- ggedit_opts$get('themeTips')
  
  lapply(names(obj.in) ,FUN = function(item){
    do.call(what = shiny::tabPanel,
            args = list(title=item,
                        shiny::fluidRow(lapply(utils::head(names(obj.in[[item]]),-1),
                                        FUN=function(subitem){
                                          x=obj.in[[item]][[subitem]]
                                          txt.in=unique(c(obj.name,item,subitem))
                                          if(obj.name=="legend"&item%in%c("position","justification")) txt.in=c(obj.name,item,subitem)
                                          obj=shiny::column(width=3,shiny::textInput(inputId = ns(paste0(c("pop",txt.in),collapse="")),
                                                                       label = x['name'],
                                                                       value = x['value'])
                                          )
                                          return(obj)
                                        })),
                        shiny::HTML(paste0(
                          paste(paste0('<b>',names(themeTips[[obj.in[[item]]$call]]),'</b>'),
                                unlist(themeTips[[obj.in[[item]]$call]]),
                                sep=': '),
                          sep="<br/>"))
            )
    )
  }
  )
}