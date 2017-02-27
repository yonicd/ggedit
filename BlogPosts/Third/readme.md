# ggedit 0.1.1: Shiny module to interactvely edit ggplots within Shiny applications

ggedit is a package that lets users interactively edit ggplot layer and theme aesthetics.  In a previous [post](https://www.r-bloggers.com/ggedit-interactive-ggplot-aesthetic-and-theme-editor/) we showed you how to use it in a collaborative workflow using standard R scripts. More importantly, we [highlighted](https://www.r-bloggers.com/ggedit-0-0-2-a-gui-for-advanced-editing-of-ggplot2-objects/) that ggedit outputs to the user, after editing, updated: gg plots, layers, scales and themes as both self-contained objects *and* script that you can paste directly in your code. 

###Installation
```r
devtools::install_github("metrumresearchgroup/ggedit",subdir="ggedit")
```

### version 0.1.1 Updates

  - **ggEdit Shiny module**: use ggedit as part of any Shiny application. 
  - **gggsave**: generalized ggsave to save multiple outputs of ggplot to single file and multiple files from a single call. Plots can be saved to various graphic devices. 

### ggEdit Shiny module

This post will demonstrate a new method to use ggedit, [Shiny modules](https://shiny.rstudio.com/articles/modules.html). A Shiny module is a chunk of Shiny code that can be reused many times in the same application, but generic enough so it can be applied in any Shiny app (in simplest terms think of it as a Shiny function). By making ggedit a Shiny module we can now replace any [renderPlot()](https://shiny.rstudio.com/reference/shiny/latest/renderPlot.html) call that inputs a ggplot and outputs in the UI [plotOutput()](https://shiny.rstudio.com/reference/shiny/latest/plotOutput.html), with an interactive ggedit layout. The analogy between how to use the ggEdit module in comparison to a standard renderPlot call can be seen in the table below. 

<center>

| |standard Shiny|Shiny Module|
|---|:---:|:---:|
|Server|output$id=renderPlot(p)|reactiveOutput=callModule(ggEdit,id,reactive(p))|
|UI|plotOutput(id)|ggEditUI(id)|

</center>

We can see that there are a few differences in the calls. To call a module you need to run a Shiny function  [callModule](https://shiny.rstudio.com/reference/shiny/latest/callModule.html), in this case ggEdit. Next, a character id for the elements the module will create in the Shiny environment and finally the arguments that are expected by the module, in this case a reactive object that outputs a ggplot or list of ggplots. This is coupled with ggEditUI, which together create a ggedit environment to edit the plots during a regular Shiny app. 

In addition to the output UI the user also gets a reactive output that has all the objects that are in the regular ggedit package (plots, layers, scales, themes) both in object and script forms. This has great advantages if you want to let users edit plots while keeping track of what they are changing. A realistic example of this would be clients (be it industry or academia) that are shown a set of default plots, with the appropriate data, and then they are given the opportunity to customize according to their specifications. Once they finish editing, the script is automatically saved to the server, updating the clients portfolio with their preferred aesthetics. No more email chains on changing a blue point to an aqua star!

Below is a small example of a static ggplot using renderPlot/plotOutput and how to call the same plot and a list of plots using ggEdit/ggeditUI. We added a small reactive text output so you can see the real-time changes of the aesthetic editing being returned to the server.

<center>
<a href="http://www.youtube.com/watch?v=pJ1kbd_OVwg" target="_blank" ><img src="http://img.youtube.com/vi/pJ1kbd_OVwg/0.jpg" alt="ggEdit Shiny module"></a>

<!---
<iframe width="560" height="315" src="https://www.youtube.com/embed/pJ1kbd_OVwg" frameborder="0" allowfullscreen></iframe>
--->


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

ui <-fluidPage(
  conditionalPanel("input.tbPanel=='tab2'",
  sidebarPanel(uiOutput('x1'),uiOutput('x2'))),
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

### gggsave

[ggsave](http://docs.ggplot2.org/0.9.2.1/ggsave.html) is the device writing function written for the ggplot2 package. A limitation of it is that only one figure could be written at a time. gggsave is a wrapper of ggsave that allows for list of ggplots to be called and then passes arguments to base graphics devices to create multiple outputs automatically, without the need of loops. 

```
library(ggedit)
#single file output to pdf
gggsave('Rplots.pdf',plot=pList)

#multiple file output to pdf
gggsave('Rplots.pdf',plot=pList,onefile=F)

#multiple file output to png
gggsave('Rplots.png',plot=pList)
```

<hr>
<em>
Jonathan Sidi joined Metrum Research Group in 2016 after working for several years on problems in applied statistics, financial stress testing and economic forecasting in both industrial and academic settings.

To learn more about additional open-source software packages developed by Metrum Research Group please visit the Metrum <a href="http://metrumrg.com/opensourcetools.html" target="_blank">website</a>.

Contact: For questions and comments, feel free to email me at: yonis@metrumrg.com or open an issue in <a href="https://github.com/metrumresearchgroup/ggedit/issues" target="_blank">github</a>.
</em>