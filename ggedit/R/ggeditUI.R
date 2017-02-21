#' @export
#' @keywords internal
ggEditUI <-function(id){
  ns<-NS(id)
  tagList(
    div(style="height:700px; border-style: inset;",
    fluidPage(
      div(class='row',
          column(width=3,actionLink(ns("updateElem"),"Update Plot Layer")),
          column(width=2,actionLink(ns("updateTheme"),"Update Plot Theme")),
          column(width=2,actionLink(ns("SetThemeGrid"),'Update Grid Theme')),
          column(width=3,actionLink(ns("SetThemeGlobal"),'Update Global Theme')),
          column(width=2,actionLink(ns('viewVerbose'),'View Layer Code'))
      ),
      hr(),
      conditionalPanel(paste0('input.',ns('viewVerbose')),uiOutput(ns("SimPrint"))),
      column(width=3,uiOutput(ns('activePlot'))),
      column(width=6,uiOutput(ns('layers'))),
      plotOutput(outputId = ns("Plot")),
      uiOutput(ns('popElems')),
      uiOutput(ns('popTheme'))
    ))
  )
}