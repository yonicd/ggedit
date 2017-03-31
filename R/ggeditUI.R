#' @title ggEditUI
#' @description The UI for the ggedit module.
#' @param id shiny module id variable
#' @export
#' @import shiny
#' @keywords internal
ggEditUI <-function(id){
  ns<-shiny::NS(id)
  shiny::tagList(
    shiny::div(style="height:700px; border-style: inset;",
    shiny::fluidPage(
      shiny::div(class='row',
                 shiny::column(width=3,shiny::actionLink(ns("updateElem"),"Update Plot Layer")),
                 shiny::column(width=2,shiny::actionLink(ns("updateTheme"),"Update Plot Theme")),
                 shiny::column(width=2,shiny::actionLink(ns("SetThemeGrid"),'Update Grid Theme')),
                 shiny::column(width=3,shiny::actionLink(ns("SetThemeGlobal"),'Update Global Theme')),
                 shiny::column(width=2,shiny::actionLink(ns('viewVerbose'),'View Layer Code'))
      ),
      shiny::hr(),
      shiny::conditionalPanel(paste0('input.',ns('viewVerbose')),shiny::uiOutput(ns("SimPrint"))),
      shiny::column(width=3,shiny::uiOutput(ns('activePlot'))),
      shiny::column(width=6,shiny::uiOutput(ns('layers'))),
      shiny::plotOutput(outputId = ns("Plot")),
      shiny::uiOutput(ns('popElems')),
      shiny::uiOutput(ns('popTheme'))
    ))
  )
}