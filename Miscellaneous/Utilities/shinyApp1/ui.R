miniPage( 
  miniContentPanel(scrollable = T,
                   fluidPage(
                     div(class='gadget-title',
                         a(href='https://github.com/metrumresearchgroup/ggedit',
                           alt="Metrum Github Dev",
                           target="_blank",h1("Edit ggplots themes and layer aesthetics")),
                         actionButton(inputId = "save"  ,label = "Save"),
                         actionButton(inputId = "done"  ,label = "Save and Close"),
                         actionButton(inputId = "exit"  ,label = "Exit"),
                         
                         img(src="http://metrumrg.com/assets/img/logo_bigger.png",style="height:40px;float:right")
                         
                         
                     ),
                     h1(),
                     fluidRow(
                       column(width=1,radioButtons(inputId = 'objRadio',label = NULL,choices = c('Example','Upload'),inline = F,selected = 'Example')),
                       conditionalPanel("input.objRadio=='Upload'",
                                        column(width=2,fileInput('obj', 'Upload Data',accept = c(
                                                                                              'application/octet-stream',
                                                                                              'rdata',
                                                                                              '.rda',
                                                                                              '.rds'
                                                                                            )
                       ))),
                       conditionalPanel("input.objRadio=='Example'",
                                        actionButton(inputId = "objBtn",label = "Upload Example")),
                       column(width=3,uiOutput('uploadObjs')),
                       column(width=2,uiOutput('plotSelect'))
                     ),
                     hr(),
                     conditionalPanel('input.uploadObjs',
                                      div(class='row',
                                          column(width=2,actionLink("updateElem","Update Plot Layer")),
                                          column(width=2,actionLink("updateTheme","Update Plot Theme")),
                                          column(width=2,actionLink("SetThemeGrid",'Update Grid Theme')),
                                          column(width=2,actionLink("SetThemeGlobal",'Update Global Theme')),
                                          column(width=2,actionLink('viewVerbose','View Layer Code')),
                                          column(width=2,downloadLink('downloadData', 'Download'))
                                      ),
                                      hr(),
                                      conditionalPanel('input.viewVerbose',uiOutput("SimPrint")),
                                      column(width=3,uiOutput('activePlot')),
                                      column(width=6,uiOutput('layers'))
                     ),
                     plotOutput(outputId = "Plot",height = "300px"),
                     uiOutput('popElems'),
                     uiOutput('popTheme')
                   )
  )
)