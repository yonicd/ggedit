if (exists(".p")) {
  p.in <- .p
} else {
  p.in <- get(".p", envir = ggedit:::.ggeditEnv)  
}

ui =fluidPage(
  #selectizeInput('p','plot',choices=1:length(plotDefault.list),selected=1),
  
  column(width=3,actionLink("updateElem","Update Plot Layer")),
  column(width=3,actionLink("updateTheme","Update Plot Theme")),
  column(width=3,actionLink("SetThemeGrid",'Update Grid Theme')),
  column(width=3,actionLink("SetThemeGlobal",'Update Global Theme')),
  uiOutput('activePlot'),
  column(width=3,selectInput("activePlot","Choose Plot:",choices = 1:length(p.in), selected = 1)),
  #column(width=12,uiOutput('Plots')),
  column(width=6,uiOutput('layers')),
  plotOutput(outputId = "Plot",height = "500px"),
  uiOutput('popElems'),
  uiOutput('popTheme')

)