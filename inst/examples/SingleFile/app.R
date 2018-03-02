library(ggedit)
library(shinyAce)

ui <- fluidPage(
  tags$head(tags$script(' var setInitialCodePosition = function() { setCodePosition(false, false); }; ')),
  conditionalPanel("input.tbPanel=='a'",
                   sidebarPanel(
                     h3('Static Plots'),
                     p('This is a standard rendered ggplot in shiny.'),
                     p('It is an image that can not be edited. You can drag it and even right click and save it.'),
                     p('But what if you wanted to change something about the aesthetics of the plot or the theme while the app is running?'),
                     p('Move to next tab ...'))
  ),
  conditionalPanel("input.tbPanel=='b'",
                   sidebarPanel(
                     h3('Interactive Plot Editing'),
                     shiny::HTML('<a href="https://www.youtube.com/watch?v=pJ1kbd_OVwg" target="_blank">Short video clip showing how to use the module</a>'),
                     p('Using ggedit modules you can edit plots (to the right) and see the updated script (below) being generated as you work!'),
                     p(),
                     p('This tab gives a basic usage example of working with one plot in the module'),
                     p('To work on multiple plots in a layout at the same time go to the next tab ...'),
                     h4('Layer Editing'),
                     p('Click on the "Update Plot Layer" link and a pop up window will appear with aesthetics relevant to the layer to edit'),
                     p(),
                     h4('Theme Editing'),
                     p('Click on the "Update Plot Theme" link and a pop up window will appear with all the theme elements to edit'),
                     p(),
                     h4('Live Script Comparisons'),
                     p(paste0('Click on the "View Layer Code" link and a text window will expand to show the original script that was used to ',
                              'create the plot layer and the new script needed to create the layer with the new aesthetic settings.')),
                     hr(),
                     p(),
                     h4('Shiny Observers'),
                     p(paste0('The ggedit modules have observers built in for shiny to use, so any changes can be acted upon within the app as the user is editing.',
                              'This can be used for auto-updating reactive documents when collaborating with many researchers or logging user activity on a company server.')),
                     uiOutput('x1'),
                     uiOutput('x2'))
  ),
  conditionalPanel("input.tbPanel=='c'",
                   sidebarPanel(
                     h3('Editing Multiple Plots'),
                     shiny::HTML('<a href="https://www.youtube.com/watch?v=pJ1kbd_OVwg" target="_blank">Short video clip showing how to use the module</a>'),
                     h4('Choosing a plot'),
                     p('When working with multiple plots use the dropdown menu to the plot you want to edit'),
                     h4('Choosing a layer'),
                     p('When choosing the plot the layers found it in will update in the radio buttons'),
                     p('If there is more than one layer in the plot choose the layer you want to edit'),
                     p(),
                     p('Click on the "Update Plot Layer" link and a pop up window will appear with aesthetics relevant to the layer to edit'),
                     p(),
                     h4('Sharing Themes'),
                     p('After choosing a plot to edit, click on the "Update Plot Theme" link and a pop up window will appear with all the theme elements to edit'),
                     p('You can share themes between plots in the layout using the "Update Grid Theme" link'),
                     p('You can also set the R session theme by selecting the "Update Global Theme" link')
                   )),
  mainPanel(
    tabsetPanel(id = 'tbPanel',
                tabPanel('renderPlot/plotOutput',value = 'a',
                         plotOutput('p')
                ),
                tabPanel('ggEdit/ggEditUI',value = 'b',
                         ggEditUI("pOut1")),
                tabPanel('ggEdit/ggEditUI with lists of plots',value = 'c',ggEditUI("pOut2"))
    )
  )
  
)

server <- function(input, output,session) {
  p1=iris%>%ggplot(aes(x=Sepal.Length,y=Sepal.Width,colour=Species))+geom_point()
  p2=iris%>%ggplot(aes(x=Sepal.Length,y=Sepal.Width))+geom_line()+geom_point((aes(colour=Petal.Width)))
  p3=list(p1=p1,p2=p2)
  
  output$p<-renderPlot({p1})
  outp1<-callModule(ggEdit,'pOut1',obj=reactive(list(p1=p1)))
  outp2<-callModule(ggEdit,'pOut2',obj=reactive(p3),showDefaults=T,height=300)
  
  output$x1<-renderUI({
    layerTxt=outp1()$UpdatedLayerCalls$p1[[1]]
    aceEditor(outputId = 'layerAce',value=layerTxt,
              mode = "r", theme = "chrome", 
              height = "100px", fontSize = 12,wordWrap = T)
  })  
  
  output$x2<-renderUI({
    
    themeTxt=outp1()$UpdatedThemeCalls$p1
    
    if(is.null(themeTxt)) themeTxt <- ' '
    
    aceEditor(outputId = 'themeAce',value=themeTxt,
              mode = "r", theme = "chrome", 
              height = "100px", fontSize = 12,wordWrap = T)
  })  
  
}

shinyApp(ui, server)