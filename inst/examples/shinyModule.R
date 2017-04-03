library(ggedit)
library(shinyAce)
ui <-fluidPage(
  conditionalPanel("input.tbPanel=='b'",
               sidebarPanel(uiOutput('x1'),uiOutput('x2'))
               ),
  mainPanel(
    tabsetPanel(id = 'tbPanel',
    tabPanel('renderPlot/plotOutput',value = 'a',plotOutput('p')),
    tabPanel('ggEdit/ggEditUI',value = 'b',ggEditUI("pOut1")),
    tabPanel('ggEdit/ggEditUI with lists of plots',value = 'c',ggEditUI("pOut2"))
  )
  )
  
)

server <- function(input, output,session) {
  p1=iris%>%ggplot(aes(x=Sepal.Length,y=Sepal.Width,colour=Species))+geom_point()
  p2=iris%>%ggplot(aes(x=Sepal.Length,y=Sepal.Width,colour=Species))+geom_line()+geom_point()
  p3=list(p1=p1,p2=p2)
  p4=iris%>%ggplot(aes(x=Sepal.Length,y=Sepal.Width,colour=Petal.Length))+geom_point()
  output$p<-renderPlot({p1})
  outp1<-callModule(ggEdit,'pOut1',obj=reactive(list(p1=p1)))
  outp2<-callModule(ggEdit,'pOut2',obj=reactive(p3),showDefaults=T)
  outp3<-callModule(ggEdit,'pOut3',obj=reactive(p4))

  output$x1<-renderUI({
    layerTxt=outp1()$UpdatedLayerCalls$p1[[1]]
            aceEditor(outputId = 'layerAce',value=layerTxt,
                      mode = "r", theme = "chrome", 
                      height = "100px", fontSize = 12,wordWrap = T)
         })  
  
  output$x2<-renderUI({
    themeTxt=outp1()$UpdatedThemeCalls$p1
    aceEditor(outputId = 'themeAce',value=themeTxt,
              mode = "r", theme = "chrome", 
              height = "100px", fontSize = 12,wordWrap = T)
  })  

}

shinyApp(ui, server)
