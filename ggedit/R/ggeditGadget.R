ggeditGadget <- function(viewer=paneViewer(minHeight = 1000),...) {
  obj <- get(".p", envir = ggedit:::.ggeditEnv)
  minHeight <- get(".minHeight", envir = ggedit:::.ggeditEnv)  
  verbose<- get(".verbose", envir = ggedit:::.ggeditEnv)  
  
    ui <-miniPage( 
      gadgetTitleBar("Edit ggplots themes and layer aesthetics"),
      miniContentPanel(
      fluidPage(
        column(width=3,actionLink("updateElem","Update Plot Layer")),
        column(width=2,actionLink("updateTheme","Update Plot Theme")),
        column(width=2,actionLink("SetThemeGrid",'Update Grid Theme')),
        column(width=3,actionLink("SetThemeGlobal",'Update Global Theme')),
        column(width=3,selectInput("activePlot","Choose Plot:",choices = split(1:length(obj),names(obj)), selected = 1)),
        column(width=6,uiOutput('layers')),
        plotOutput(outputId = "Plot",height = "300px"),
        uiOutput('popElems'),
        uiOutput('popTheme')
      )
      )
    )

    server = function(input, output, session) {
        #Plots----
        
        objList.new<<- obj 

        plotIdx=reactive({
          if(is.null(input$activePlot)){
            1
          }else{
            as.numeric(input$activePlot)
          }
        })

        observe(obj.new<<-objList.new[[plotIdx()]])

        theme.now=theme_get()
        obj.theme<<-lapply(objList.new,function(p){
          if(length(p$theme)>0) theme.now=theme.now+p$theme
          themeFetch(theme.now)
        })

#Layers----
        output$layers=renderUI({
          obj.new<<-objList.new[[as.numeric(input$activePlot)]]
          radioButtons("geoms","Choose layer(s):",choices = geom_list(obj.new),selected = geom_list(obj.new)[1],inline = T)
        })

        update.Layer=eventReactive(input$sendElem,{
          obj.new<<-objList.new[[as.numeric(input$activePlot)]]
          layer.idx=which(geom_list(obj.new)==input$geoms)
          numElem=unlist(lapply(obj.Elems[[layer.idx]],function(x) length(x$val[[1]])))
          for(item in names(obj.Elems[[layer.idx]])){
            if(numElem[item]==1) {
              newLayer=cloneLayer(obj.new$layers[[layer.idx]])
              newLayer$aes_params[[item]]=eval(parse(text=paste0('input$pop',toupper(item))))
              #obj.new$layers[[layer.idx]]<<-NULL
              obj.new$layers[[layer.idx]]<<-newLayer
            }else{
              if(obj.Elems[[layer.idx]][[item]][['class']][[1]]=='numeric'){
                palItem=paste0("'",eval(parse(text=paste0("input$pop",toupper(item)))),"'")
                suppressMessages(eval(parse(text=paste0("obj.new<<-obj.new+scale_",item,"_gradientn(colours=brewer_pal(palette=",palItem,",direction=-1)(9)[1:5])"))))
              }else{
                vals=unlist(lapply(names(input)[grepl(paste0('pop',toupper(item),'[1-9]'),names(input))],function(x) input[[x]]))
                if(!item%in%c('size','shape','linetype')) vals=paste0("'",vals,"'")
                if(item=='linetype') {
                  vals=match(vals,c('0',linetype_pal()(6)))-1
                }

                vals=paste0(vals,collapse=',')
                suppressMessages(eval(parse(text=paste0("obj.new<<-obj.new+scale_",item,"_manual(values=c(",vals,"))"))))
              }
            }
          }
          objList.new[[as.numeric(input$activePlot)]]<<-obj.new
          return(objList.new)
        })

        output$popElems=renderUI({
          obj.new<<-objList.new[[as.numeric(input$activePlot)]]
          obj.Elems<<-fetch_aes_ggplotBuild(obj.new,geom_list(obj.new))
          if(is.null(input$geoms)){
            gIdx=1
          }else{
            gIdx=input$geoms
            }
          obj.elems=obj.Elems[[gIdx]]

          arg.value=function(item){
            item_class=obj.elems[[item]]$class[[1]]
            if(item_class=='data.frame'){
              if(item%in%c('colour','color','fill')){
                x=aesColour(item)
                y=obj.elems[[item]]$val[[1]]
                if(!grepl("[#]",y)) y=col2hcl(y)
                x$args$value=colourpicker:::closestColHex(y)[1]
                x=list(x=x)
              }else{
                x=aesSlide(item)
                x$args$value=obj.elems[[item]]$val[[1]]
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
                  stop("non colour aesthetics of numeric inputs are not currently supported in ggedit", call. = FALSE)
                }
              }
              if(item_class%in%c('character','factor')){
                x=lapply(obj.elems[[item]]$val[[1]],function(y){
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
                    lapply(names(obj.elems)[!names(obj.elems)%in%c('family')] ,FUN = function(item){
                      list(
                        column(width = 3,
                               lapply(arg.value(item),function(x) {
                                 do.call(what = x[['type']],args = x[['args']])
                               }))
                      )
                    })),
                  div(align="right",actionButton("sendElem","Update Layer"))
                           
                           


          )
        })
#Theme----
        update.Theme=eventReactive(input$sendTheme,{
          obj.new<<-objList.new[[as.numeric(input$activePlot)]]

          strThemeCallList=lapply(names(obj.theme[[plotIdx()]]),function(item){
            themeNewVal(obj.theme[[plotIdx()]][item],obj.new,input)
          })

          strThemeCall=paste0("obj.new<<-obj.new+theme(",paste0(unlist(strThemeCallList),collapse = ","),")")
          
          eval(parse(text=strThemeCall))
          objList.new[[as.numeric(input$activePlot)]]<<-obj.new

          themeUpdate<<-lapply(objList.new,function(p) p$theme)
          return(objList.new)
        })

        observeEvent(input$SetThemeGlobal,{
          if(length(obj.new$theme)>0) theme.now=theme.now+obj.new$theme
          theme_set(theme_get()%+replace%theme.now)
        })

        update.ThemeGrid=eventReactive(input$SetThemeGrid,{
          p.now<<-objList.new[[as.numeric(input$activePlot)]]
          if(length(p.now$theme)>0) theme.now=theme.now+p.now$theme

          for(i in 1:length(objList.new)) objList.new[[i]]<<- objList.new[[i]]+theme.now

          return(objList.new)
        })

        output$popTheme=renderUI({
          bsModal(id = "updateThemePopup", title = HTML('Update Plot Theme <a href="http://docs.ggplot2.org/0.9.3.1/theme.html" target="_blank">(help)</a>'), trigger = "updateTheme", size = "large",

                  do.call(tabsetPanel,

                          unlist(lapply(1:length(obj.theme[[plotIdx()]]),FUN = function(j){
                            if(themeListDepth(obj.theme[[plotIdx()]][j])>2){
                              list(themeMakePanel(obj.theme[[plotIdx()]][j]))
                            }else{
                              unlist(lapply(j, function(i) {themeMakePanel(obj.theme[[plotIdx()]][i])}),F)}
                          }),F)


                  ),
                  hr(),
                  div(align="right",actionButton("sendTheme","Set Theme"))


          )
        })

        #Render Plot----
        output$Plot=renderPlot({
          plot.ggedit(objList.new)
        },height=minHeight*.6)

        observeEvent(input$updateElem,{
          output$Plot=renderPlot({
            if(input$sendElem==0){
              plot.ggedit(objList.new)
            }else{
              pList.out=update.Layer()
              plot.ggedit(pList.out)
            }
          },height=minHeight*.6)
        })

        observeEvent(input$updateTheme,{
          output$Plot=renderPlot({
            if(input$sendTheme==0){
              plot.ggedit(objList.new)
            }else{
              pList.out=update.Theme()
              plot.ggedit(pList.out)
            }
          },height=minHeight*.6)
        })

        observeEvent(input$SetThemeGrid,{
          pList.out=update.ThemeGrid()
          output$Plot=renderPlot({plot.ggedit(pList.out)},height=minHeight*.6)
        })

        observeEvent(input$done, {
          UpdatedPlots=objList.new
          class(UpdatedPlots)=c("ggedit",class(UpdatedPlots))
          
          ggeditOut=list(UpdatedPlots=UpdatedPlots,
                         UpdatedLayers=layersListObj(obj = objList.new,lbl=names(objList.new)),
                         UpdatedLayersElements=layersList(objList.new)
                         )
          
          if(verbose) ggeditOut$UpdatedLayerCalls=lapply(objList.new,function(p) lapply(p$layer,function(item) cloneLayer(l = item,verbose = T)))
          
          if(exists('themeUpdate',envir = .GlobalEnv)) {
            ggeditOut$UpdatedThemes=themeUpdate
            if(verbose){
              ggeditOut$UpdatedThemeCalls=lapply(objList.new,function(p,input){
                if(length(p$theme)>0){
                x.theme=themeFetch(p$theme)
                x=lapply(names(x.theme),function(item){themeNewVal(x.theme[item],p,input)})
                paste0("theme(",paste0(unlist(x),collapse = ","),")")
                }else{
                c('list()')
                }
              },input)
            } 
            }
          
          
          
          class(ggeditOut)=c("ggedit",class(ggeditOut))
          
          rm(list = ls(envir = .GlobalEnv)[ls(envir = .GlobalEnv)%in%c('obj.new','obj.theme','objList.new','obj.Elems','themeUpdate')],envir = .GlobalEnv)
          stopApp(ggeditOut)
        })
        
        observeEvent(input$cancel,{
          rm(list = ls(envir = .GlobalEnv)[ls(envir = .GlobalEnv)%in%c('obj.new','obj.theme','objList.new','obj.Elems','themeUpdate')],envir = .GlobalEnv)
          stopApp(NULL)
        })
        
    }
    
    runGadget(ui, server, stopOnCancel = FALSE, viewer = viewer)
  

}
