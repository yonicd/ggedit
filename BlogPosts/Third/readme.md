# ggedit 0.1.1: shiny module to interactvely edit ggplots within shiny applications

ggedit is a package that lets users interactively edit ggplot layer and theme aesthetics.  In previous [posts](https://www.r-bloggers.com/ggedit-interactive-ggplot-aesthetic-and-theme-editor/) we showed how you can use it in a collaborative workflow using standard R scripts. More importantly we [highlighted](https://www.r-bloggers.com/ggedit-0-0-2-a-gui-for-advanced-editing-of-ggplot2-objects/) that ggedit outputs to the user, after editing, updated: gg plots, layers, scales and themes as both self contained objects *and* script that you can paste directly in your code. 

###Installation
```r
devtools::install_github("metrumresearchgroup/ggedit",subdir="ggedit")
```

### version 0.1.1 Updates

  - **ggEdit shiny module**: use ggedit as part of any shiny application. 
  - **gglsave**: generalization of ggsave to save multiple outputs of ggplot to single file and multiple files in a single call to various graphics devices. 

### ggEdit shiny module

This post will discuss a new method to use ggedit, [shiny modules](https://shiny.rstudio.com/articles/modules.html). A shiny module is a chunk of shiny code that can be reused many times in the same application, but generic enough so it can be applied in any shiny app (in simplest terms think of it as a shiny function). By making ggedit a shiny module we can now replace any [renderPlot()](https://shiny.rstudio.com/reference/shiny/latest/renderPlot.html) call that inputs a ggplot and outputs in the UI [plotOutput()](https://shiny.rstudio.com/reference/shiny/latest/plotOutput.html), with an interactive ggedit layout. The analogy between how to use the ggEdit module in comparison to a standard renderPlot call can be seen in the table below. 

<center>

| |standard Shiny|Shiny Module|
|---|:---:|:---:|
|Server|output$id=renderPlot(p)|reactiveOutput=callModule(ggEdit,id,reactive(p))|
|UI|plotOutput(id)|ggEditUI(id)|

</center>

We can see that there are a few differences in the calls. To call a module you need to run a shiny function  [callModule](https://shiny.rstudio.com/reference/shiny/latest/callModule.html), in this case ggEdit. Next a character id for the elements the module will create in the shiny enviornment and finally the arguments that are expected by the module, in this case a reactive object that outputs a ggplot or list of ggplots. This is coupled with ggEditUI which together create a ggedit enviornment to edit the plots during a regular shiny app. 

In addition to the output UI the user also gets a reactive output that has all the objects that are in the regular ggedit package (plots, layers, scales, themes) both in object and script forms. This has great advantages if you want to let users edit plots, while keeping track of what they are changing. A realistic example of this would be clients (be it industry or academia) that are shown a set of default plots, with the appropriate data, and then are given the opportunity to customize to their needs and wants. Once they finish editing, the script is automatically saved to the server updating the clients portfolio with their prefered aesthetics. No more email chains on changing a blue point to a aquagreen star!

Below is a small example of a static ggplot using renderPlot/plotOutput and how to call the same plot and a list of plots using ggEdit/ggeditUI. We added a small reactive text output so you can see the real time changes of the aesthetic editing being returned to the server.

<center>
<a href="http://www.youtube.com/watch?v=pJ1kbd_OVwg" target="_blank" ><img src="http://img.youtube.com/vi/pJ1kbd_OVwg/0.jpg" alt="ggEdit shiny module"></a>
</center>

### Source Code for example
```r
library(ggedit)

server <- function(input, output,session) {
  p1=ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width,colour=Species))+geom_point()
  p2=ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width,colour=Species))+geom_line()+geom_point()
  p3=list(p1=p1,p2=p2)

  output$p<-renderPlot({p1})
  outp1<-callModule(ggEdit,'pOut1',obj=reactive(list(p1=p1)))
  outp2<-callModule(ggEdit,'pOut2',obj=reactive(p3))

  output$lAce<-renderUI({
    layerTxt=outp1()$UpdatedLayerCalls$p1[[1]]
            aceEditor(outputId = 'layerAce',value=layerTxt,
                      mode = "r", theme = "chrome", 
                      height = "100px", fontSize = 12,wordWrap = T)
         })  
  
  output$tAce<-renderUI({
    themeTxt=outp1()$UpdatedThemeCalls$p1
            aceEditor(outputId = 'themeAce',value=themeTxt,
                      mode = "r", theme = "chrome", 
                      height = "100px", fontSize = 12,wordWrap = T)
  })  

}

ui <-fluidPage(
  conditionalPanel("input.tbPanel=='tab2'",
  sidebarPanel(uiOutput('lAce'),uiOutput('tAce'))),
  mainPanel(
    tabsetPanel(id = 'tbPanel',
      tabPanel('renderPlot/plotOutput',value = 'tab1',plotOutput('p')),
      tabPanel('ggEdit/ggEditUI',value = 'tab2',ggEditUI("pOut1")),
      tabPanel('ggEdit/ggEditUI with lists of plots',value = 'tab3',ggEditUI("pOut2"))
      )
    )
  )


shinyApp(ui, server)

```
<hr>
<em>
Jonathan Sidi joined Metrum Researcg Group in 2016 after working for several years on problems in applied statistics, financial stress testing and economic forecasting in both industrial and academic settings.

To learn more about additional open-source software packages developed by Metrum Research Group please visit the Metrum <a href="http://metrumrg.com/opensourcetools.html" target="_blank">website</a>.

Contact: For questions and comments, feel free to email me at: yonis@metrumrg.com or open an issue in <a href="https://github.com/metrumresearchgroup/ggedit/issues" target="_blank">github</a>.
</em>