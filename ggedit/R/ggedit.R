ggedit <- function(p.in,viewer=paneViewer(minHeight = 1000),...) {
  
  if(!Sys.getenv("RSTUDIO") == "1") viewer=browserViewer()
  
  if(is.ggplot(p.in)) p.in=list(p.in)
  
  if(!all(unlist(lapply(p.in,is.ggplot)))) stop("'object' is not a valid ggplot object")
  
  if(!exists('minHeight')) minHeight=1000
  
  if(deparse(substitute(viewer))=='paneViewer()') viewer=paneViewer(minHeight)
  
    ui <-miniPage( 
      gadgetTitleBar("Edit ggplots themes and layer aesthetics"),
      miniContentPanel(
      fluidPage(
        column(width=3,actionLink("updateElem","Update Plot Layer")),
        column(width=3,actionLink("updateTheme","Update Plot Theme")),
        column(width=3,actionLink("SetThemeGrid",'Update Grid Theme')),
        column(width=3,actionLink("SetThemeGlobal",'Update Global Theme')),
        column(width=3,selectInput("activePlot","Choose Plot:",choices = 1:length(p.in), selected = 1)),
        column(width=6,uiOutput('layers')),
        plotOutput(outputId = "Plot",height = "300px"),
        uiOutput('popElems'),
        uiOutput('popTheme')
      )
      )
    )
    
    server = function(input, output, session) {
        #Plots----

        pList.new<<-p.in

        plotIdx=reactive({
          if(is.null(input$activePlot)){
            1
          }else{
            as.numeric(input$activePlot)
          }
        })

        observe(p.new<<-pList.new[[plotIdx()]])

        theme.now=theme_get()
        p.theme<<-lapply(pList.new,function(p){
          if(length(p$theme)>0) theme.now=theme.now+p$theme
          themeFetch(theme.now)
        })

        #Layers----
        output$layers=renderUI({
          p.new<<-pList.new[[as.numeric(input$activePlot)]]
          radioButtons("geoms","Choose layer(s):",choices = geom_list(p.new),selected = geom_list(p.new)[1],inline = T)
        })

        update.Layer=eventReactive(input$sendElem,{
          p.new<<-pList.new[[as.numeric(input$activePlot)]]
          layer.idx=which(geom_list(p.new)==input$geoms)
          numElem=unlist(lapply(p.Elems[[layer.idx]],function(x) length(x$val[[1]])))
          for(item in names(p.Elems[[layer.idx]])){
            if(numElem[item]==1) {
              p.new$layers[[layer.idx]]$aes_params[[item]]=eval(parse(text=paste0('input$pop',toupper(item))))
            }
            else{
              if(p.Elems[[layer.idx]][[item]][['class']][[1]]=='numeric'){
                palItem=paste0("'",eval(parse(text=paste0("input$pop",toupper(item)))),"'")
                suppressMessages(eval(parse(text=paste0("p.new<<-p.new+scale_",item,"_gradientn(colours=brewer_pal(palette=",palItem,",direction=-1)(9)[1:5])"))))
              }else{
                vals=unlist(lapply(names(input)[grepl(paste0('pop',toupper(item),'[1-9]'),names(input))],function(x) input[[x]]))
                if(!item%in%c('size','shape','linetype')) vals=paste0("'",vals,"'")
                if(item=='linetype') {
                  vals=match(vals,c('0',linetype_pal()(6)))-1
                }

                vals=paste0(vals,collapse=',')
                suppressMessages(eval(parse(text=paste0("p.new<<-p.new+scale_",item,"_manual(values=c(",vals,"))"))))
              }
            }
          }
          pList.new[[as.numeric(input$activePlot)]]<<-p.new
          return(pList.new)
        })

        output$popElems=renderUI({
          p.new<<-pList.new[[as.numeric(input$activePlot)]]
          p.Elems<<-fetch_aes_ggplotBuild(p.new,geom_list(p.new))
          if(is.null(input$geoms)){
            gIdx=1
          }else{
            gIdx=input$geoms
            }
          p.elems=p.Elems[[gIdx]]

          arg.value=function(item){

            item_class=p.elems[[item]]$class[[1]]
            if(item_class=='data.frame'){
              if(item%in%c('colour','color','fill')){
                x=aesColour(item)
                y=p.elems[[item]]$val[[1]]
                if(!grepl("[#]",y)) y=col2hcl(y)
                x$args$value=colourpicker:::closestColHex(y)[1]
                x=list(x=x)
              }else{
                x=aesSlide(item)
                x$args$value=p.elems[[item]]$val[[1]]
                if(item=='alpha'){
                  if(is.na(x$args[['value']])) x$args[['value']]=1
                }
                x=list(x=x)
              }
            }else{
              if(item_class=='numeric'){
                if(item%in%c('colour','color','fill')){
                  x=aesSelect(item)
                  x$args$selected='Blues'
                  x$args$choices=c(NA,'Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges', 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd')
                  x=list(x=x)
                }else{

                }
              }
              if(item_class%in%c('character','factor')){
                x=lapply(p.elems[[item]]$val[[1]],function(y){
                  if(item%in%c('colour','color','fill')){
                    z=aesColour(item)
                    if(!grepl("[#]",y)) y=col2hcl(y)
                    z$args$value=colourpicker:::closestColHex(y)[1]
                  }else{
                    z=aesSelect(item)
                    if(is.numeric(y)&!item%in%c('size','shape')){
                      eval(parse(text=paste0('z$args$selected=c(',item,'_pal()(6)[',y,'])')))
                    }else{
                      z$args$selected=y
                    }

                    if(item=='shape') {
                      z$args$choices=c(0:25)
                    }
                    if(item=='size') z$args$choices=c(0:10)
                    if(!item%in%c('shape','size')) eval(parse(text=paste0('z$args$choices=c(0,',item,'_pal()(6))')))
                  }

                  return(z)
                })

                for(i in 1:length(x)){
                  x[[i]]$args$label=paste0(x[[i]]$args$label,": Group ",i)
                  x[[i]]$args$inputId=paste0(x[[i]]$args$inputId,i)
                }
              }
            }

            return(x)
          }

          bsModal(id = "updateElemPopup", title = "Update Plot Layer", trigger = "updateElem", size = "large",

                  fluidRow(
                    lapply(names(p.elems) ,FUN = function(item){
                      list(
                        column(width = 3,
                               lapply(arg.value(item),function(x) {
                                 do.call(what = x[['type']],args = x[['args']])
                               }))
                      )
                    })),
                  fluidRow(column(width=1,actionButton("sendElem","Go")))


          )
        })
        #Theme----
        update.Theme=eventReactive(input$sendTheme,{
          p.new<<-pList.new[[as.numeric(input$activePlot)]]

          strThemeCallList=lapply(names(p.theme[[plotIdx()]]),function(item){
            themeNewVal(p.theme[[plotIdx()]][item],p.new,input)
          })

          strThemeCall=paste0("p.new<<-p.new+theme(",paste0(unlist(strThemeCallList),collapse = ","),")")
          eval(parse(text=strThemeCall))
          pList.new[[as.numeric(input$activePlot)]]<<-p.new

          themeUpdate<<-lapply(pList.new,function(p) p$theme)
          #if(output.in) ggeditOut$UpdatedThemes<<-themeUpdate
          return(pList.new)
        })

        observeEvent(input$SetThemeGlobal,{
          if(length(p.new$theme)>0) theme.now=theme.now+p.new$theme
          theme_set(theme_get()%+replace%theme.now)
        })

        update.ThemeGrid=eventReactive(input$SetThemeGrid,{
          p.now<<-pList.new[[as.numeric(input$activePlot)]]
          if(length(p.now$theme)>0) theme.now=theme.now+p.now$theme

          for(i in 1:length(pList.new)) pList.new[[i]]<<- pList.new[[i]]+theme.now

          return(pList.new)
        })

        output$popTheme=renderUI({
          bsModal(id = "updateThemePopup", title = "Update Plot Theme", trigger = "updateTheme", size = "large",

                  do.call(tabsetPanel,

                          unlist(lapply(1:length(p.theme[[plotIdx()]]),FUN = function(j){
                            if(themeListDepth(p.theme[[plotIdx()]][j])>2){
                              list(themeMakePanel(p.theme[[plotIdx()]][j]))
                            }else{
                              unlist(lapply(j, function(i) {themeMakePanel(p.theme[[plotIdx()]][i])}),F)}
                          }),F)


                  ),
                  hr(),
                  actionButton("sendTheme","Set Theme")


          )
        })

        #Render Plot----
        output$Plot=renderPlot({
          pListPrint(pList.new)
        },height=minHeight*.6)

        observeEvent(input$updateElem,{
          output$Plot=renderPlot({
            if(input$sendElem==0){
              pListPrint(pList.new)
            }else{
              pList.out=update.Layer()
              pListPrint(pList.out)
            }
          },height=minHeight*.6)
        })

        observeEvent(input$updateTheme,{
          output$Plot=renderPlot({
            if(input$sendTheme==0){
              pListPrint(pList.new)
            }else{
              pList.out=update.Theme()
              pListPrint(pList.out)
            }
          },height=minHeight*.6)
        })

        observeEvent(input$SetThemeGrid,{
          pList.out=update.ThemeGrid()
          output$Plot=renderPlot({pListPrint(pList.out)},height=minHeight*.6)
        })

        observeEvent(input$done, {
          ggeditOut=list(UpdatedPlots=pList.new,UpdatedLayerElements=layersList(pList.new))
          if(exists('themeUpdate',envir = .GlobalEnv)) ggeditOut$UpdatedThemes=themeUpdate
          rm(list = ls(envir = .GlobalEnv)[ls(envir = .GlobalEnv)%in%c('p.new','p.theme','pList.new','p.Elems','themeUpdate')],envir = .GlobalEnv)
          stopApp(ggeditOut)
        })
        
        observeEvent(input$cancel,{
          rm(list = ls(envir = .GlobalEnv)[ls(envir = .GlobalEnv)%in%c('p.new','p.theme','pList.new','p.Elems','themeUpdate')],envir = .GlobalEnv)
          stopApp(NULL)
        })
        
    }
    
    runGadget(ui, server, stopOnCancel = FALSE, viewer = viewer)
  

}
