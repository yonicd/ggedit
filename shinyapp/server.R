server = function(input, output, session) {
  #Import Handling----
  #logical set to true to pass to cloneLayer calls
  verbose=T
  
  #get the list of plots from the upload enviornment
  objList.new<<-get('p.in',uploadEnv)

  #observe events triggered by load example button
  observeEvent(input$objBtn,{
    
    #load example into uploadEnv
    load('example.rda',envir = uploadEnv)
    
    #populate a selectInput for the names of objects in the rda
    output$uploadObjs=renderUI({
      objNames=ls(envir = uploadEnv)
      selectInput(inputId = 'uploadObjs',label = 'Objects from Upload Environment',choices = objNames)
    })
    
    #given a chosen object populate selectInput for the plot names in the list
    output$plotSelect=renderUI({
      
      if(is.null(input$uploadObjs)){
        nm=ls(envir = uploadEnv)[1]
      }else{
        nm=input$uploadObjs
      }
      x=get(nm, envir = uploadEnv)
      
      if(is.null(names(x))) names(x)=as.character(1:length(x))
      selectizeInput(inputId = 'plotSelect',label = 'Plots in Selected Object',choices = names(x),multiple = T,options=list(plugins=list('drag_drop','remove_button')))
    })
  })
  
  #observe events triggered by upload button
  observeEvent(input$obj,{
    #import an object from file
    load(input$obj$datapath,envir = uploadEnv)
    
    #populate a selectInput with the names of objects in the rda
    output$uploadObjs=renderUI({
      objNames=ls(envir = uploadEnv)
      selectInput(inputId = 'uploadObjs',label = 'Objects from Upload Environment',choices = objNames)
    })
    
    #populate a selectInput with the names of the plots in the chosen object
    output$plotSelect=renderUI({
      if(is.null(input$uploadObjs)){
        nm=ls(envir = uploadEnv)[1]
      }else{
        nm=input$uploadObjs
      }
      
      x=get(nm, envir = uploadEnv)

      if(is.null(names(x))) names(x)=as.character(1:length(x))
      selectizeInput(inputId = 'plotSelect',label = 'Plots in Selected Object',choices = names(x),multiple = T,options=list(plugins=list('drag_drop','remove_button')))
    })
  })
  
  #populate select with the plots chosen to work on
  output$activePlot=renderUI({
    if(!is.null(input$uploadObjs)){
      objList.imp<-get(input$uploadObjs, envir = uploadEnv)
      if(is.null(names(objList.imp))) names(objList.imp)=as.character(1:length(objList.imp))
      if(!is.null(input$plotSelect)) objList.imp<-objList.imp[input$plotSelect]
      objList.new<<-objList.imp
    } 

    nonLayers<<-vector('list',length(objList.new))
    nonLayersTxt<<-vector('list',length(objList.new))
        
    nm=factor(names(objList.new),ordered = T,levels=names(objList.new))
    
    selectInput("activePlot","Choose Plot:",choices = split(1:length(nm),nm))
  })
  
  #loop through the plots in the imported object and save the initial calls of each layer
  baseLayerVerbose=eventReactive(input$uploadObjs,{
    objList.imp<-get(input$uploadObjs, envir = uploadEnv)
    lapply(objList.imp,function(x) lapply(x$layers,function(y) cloneLayer(y,verbose = T))) 
  })
  
  #when activePlot changes update the shinyAce editor
  observeEvent(input$activePlot,{
      output$SimPrint <- renderUI({
              junk=''
              if(length(simTxt())>0) junk=textConnection(capture.output(simTxt()))
              toace=paste0(readLines(junk),collapse='\n')
              if(input$viewVerbose%%2==1){
                if (Sys.info()[1] == "Windows"){
                  output$codeout<-renderText({toace})  
                  verbatimTextOutput('codeout')
                }else{
                  aceEditor(outputId = "codeout",value=toace,mode = "r", theme = "chrome", height = "100px", fontSize = 12) 
                }
              } 
            })  
  })
  
  #index of the active plot chosen 
  plotIdx=reactive({
    if(is.null(input$activePlot)){
      1
    }else{
      as.numeric(input$activePlot)
    }
  })
  
  #conditional on any changes to uploadObjs reset the plotIdx
  plotIdx<-eventReactive(input$uploadObjs,{
    if(is.null(input$activePlot)){
      1
    }else{
      as.numeric(input$activePlot)
    }
  })
  
  #set obj.new to the active plot
  observe(obj.new<<-objList.new[[plotIdx()]])
  
  #set theme.new to the session theme
  theme.now=theme_get()
  
  #get all the current themes from the plots
  obj.theme<<-lapply(objList.new,function(p){
    if(length(p$theme)>0) theme.now=theme.now+p$theme
    themeFetch(theme.now)
  })
  
  
  #Layers----
  #return layers in a plot to radio buttons to choose which one to edit
  observeEvent(input$plotSelect,{
    output$layers=renderUI({
      if(is.null(input$activePlot)){
        aP=1
      }else{
        aP=as.numeric(input$activePlot)
      } 
      obj.new<<-objList.new[[aP]]
      radioButtons("geoms","Choose layer(s):",choices = geom_list(obj.new),selected = geom_list(obj.new)[1],inline = T)
    })    
  })
  
  #main function that updates the layers
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
                #browser()
                if(input[[paste0('pop',toupper(item),'fixedPal')]]!='Manual'){
                  palItem=paste0("'",input[[paste0('pop',toupper(item),'fixedPal')]],"'")
                  palTxt=paste0("scale_",item,"_gradientn(colours=brewer_pal(palette=",palItem,",direction=-1)(9)[1:5])")
                  nonLayersTxt[[as.numeric(input$activePlot)]][[paste0("scale_",item,"_gradientn")]]<<-palTxt
                  suppressMessages({nL=eval(parse(text=palTxt))})
                  nonLayers[[as.numeric(input$activePlot)]][[paste0("scale_",item,"_gradientn")]]<<-nL
                  suppressMessages({eval(parse(text=paste0("obj.new<<-obj.new+",palTxt)))})
                }else{
                  LowCol=paste0("'",input[[paste0('pop',input$pop,toupper(item),'Low')]],"'")
                  HighCol=paste0("'",input[[paste0('pop',input$pop,toupper(item),'High')]],"'")
                  ColTxt=paste0("scale_",item,"_gradient(low=",LowCol,",high=",HighCol,")")                  
                  nonLayersTxt[[as.numeric(input$activePlot)]][[paste0("scale_",item,"_gradient")]]<<-ColTxt
                  suppressMessages({nL=eval(parse(text=ColTxt))})
                  nonLayers[[as.numeric(input$activePlot)]][[paste0("scale_",item,"_gradient")]]<<-nL
                  suppressMessages({eval(parse(text=paste0("obj.new<<-obj.new+",ColTxt)))})
                  }
              }else{
                vals=unlist(lapply(names(input)[grepl(paste0('pop',toupper(item),'[1-9]'),names(input))],function(x) input[[x]]))
                if(!item%in%c('size','shape','linetype')) vals=paste0("'",vals,"'")
                if(item=='linetype') {
                  vals=match(vals,c('0',linetype_pal()(6)))-1
                }

                vals=paste0(vals,collapse=',')
                nonLayersTxt[[as.numeric(input$activePlot)]][[paste0("scale_",item,"_manual")]]<<-paste0("scale_",item,"_manual(values=c(",vals,"))")
                suppressMessages({nL=eval(parse(text=paste0("scale_",item,"_manual(values=c(",vals,"))")))})
                nonLayers[[as.numeric(input$activePlot)]][[paste0("scale_",item,"_manual")]]<<-nL
                suppressMessages(eval(parse(text=paste0("obj.new<<-obj.new+scale_",item,"_manual(values=c(",vals,"))"))))
              }
            }
          }
          objList.new[[as.numeric(input$activePlot)]]<<-obj.new
          return(objList.new)
        })

  #populate the elements in the modal UI
  output$popElems=renderUI({
    if(is.null(input$activePlot)){
      aP=1
    }else{
      aP=as.numeric(input$activePlot)
    } 
    obj.new<<-objList.new[[aP]]
    
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
                  x=vector('list',2)
                  names(x)=c('type','args')
                  x[['type']]=aesColourCont
                  x$args=list(type=item)
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
  #updates the theme of the active plot
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
  
  #updates the session theme
  observeEvent(input$SetThemeGlobal,{
    if(length(obj.new$theme)>0) theme.now=theme.now+obj.new$theme
    theme_set(theme_get()%+replace%theme.now)
  })
  
  #copies the theme from the active plot to the rest of the plots
  update.ThemeGrid=eventReactive(input$SetThemeGrid,{
    p.now<<-objList.new[[as.numeric(input$activePlot)]]
    if(length(p.now$theme)>0) theme.now=theme.now+p.now$theme
    
    for(i in 1:length(objList.new)) objList.new[[i]]<<- objList.new[[i]]+theme.now
    
    return(objList.new)
  })
  
  #modal UI for the theme
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
  
  #Sim Text ----
  #handles the verbose outputs
  simTxt=reactive({
          LayerVerbose<-lapply(objList.new,function(p) lapply(p$layer,function(item) cloneLayer(l = item,verbose = T)))
          if(is.null(input$activePlot)){
            aP=1
          }else{
            aP=as.numeric(input$activePlot)
          } 
          
          if(is.null(input$geoms)){
            l=1
          }else{
            l=which(geom_list(obj.new)==input$geoms)
          }
          
          a=input$updateElem
          a1=input$updateElemPopup
          
          if(length(l)==0) l=1
          strNew=strBase=''
          if(length(LayerVerbose)>0) strNew=LayerVerbose[[aP]][[l]]
          if(length(baseLayerVerbose())>0) strBase=baseLayerVerbose()[[aP]][[l]]
          return(list(Original=strBase,Edited=strNew))
        })
  
  #Render Plot----
  #initial default plot
  output$Plot=renderPlot({
    plot(as.ggedit(objList.new))
  },height=minHeight*.6)
  
  #resets the plot if new rda is uploaded
  observeEvent(input$uploadObjs,{
    output$Plot=renderPlot({
      plot(as.ggedit(objList.new))
    },height=minHeight*.6)
  })
  
  #resets the plot if new active plot is selected
  observeEvent(input$plotSelect,{
    output$Plot=renderPlot({
      plot(as.ggedit(objList.new))
    },height=minHeight*.6)
  })
  
  #resets the plot if a layer is updated
  observeEvent(input$updateElem,{
    output$Plot=renderPlot({
      if(input$sendElem==0){
        plot(as.ggedit(objList.new))
      }else{
        pList.out=update.Layer()
        plot(as.ggedit(pList.out))
      }
    },height=minHeight*.6)
  })
  
  #resets the plot if a theme is updated
  observeEvent(input$updateTheme,{
    output$Plot=renderPlot({
      if(input$sendTheme==0){
        plot(as.ggedit(objList.new))
      }else{
        pList.out=update.Theme()
        plot(as.ggedit(pList.out))
      }
    },height=minHeight*.6)
  })
  
  #resets the plot if a theme is copied to the other plots
  observeEvent(input$SetThemeGrid,{
    pList.out=update.ThemeGrid()
    output$Plot=renderPlot({plot(as.ggedit(pList.out))},height=minHeight*.6)
  })
  
  #Close App options ----
  observeEvent(input$done, {
    UpdatedPlots=objList.new
    class(UpdatedPlots)=c("ggedit",class(UpdatedPlots))
    
    ggeditOut=list(UpdatedPlots=UpdatedPlots,
                   UpdatedLayers=layersListObj(obj = objList.new,lbl=names(objList.new)),
                   UpdatedLayersElements=layersList(objList.new)
                   )

    if(verbose) ggeditOut$UpdatedLayerCalls=lapply(objList.new,function(p) lapply(p$layer,function(item) cloneLayer(l = item,verbose = T)))
              
    names(nonLayers)<<-names(nonLayersTxt)<<-names(objList.new)
    ggeditOut$updatedScales=nonLayers

    if(verbose) ggeditOut$UpdatedScalesCalls=nonLayersTxt            
    
    
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
    
    rm(list = ls(envir = .GlobalEnv)[ls(envir = .GlobalEnv)%in%c('obj.new','obj.theme','objList.new','obj.Elems','themeUpdate','nonLayers','nonLayersTxt')],envir = .GlobalEnv)
    save(ggeditOut,file=file.path(getwd(),'outputs/ggeditOutput.rdata'))
    stopApp(paste('Output saved to',file.path(getwd(),'outputs/ggeditOutput.rdata')))
  })
  
  observeEvent(input$save, {
    UpdatedPlots=objList.new
    class(UpdatedPlots)=c("ggedit",class(UpdatedPlots))
    ggeditOut=list(UpdatedPlots=UpdatedPlots,UpdatedLayers=layersListObj(obj = objList.new,lbl=names(objList.new)),UpdatedLayersElements=layersList(objList.new))
    class(ggeditOut)=c("ggedit",class(ggeditOut))
    if(exists('themeUpdate',envir = .GlobalEnv)) ggeditOut$UpdatedThemes=themeUpdate
    save(ggeditOut,file=file.path(getwd(),'outputs/ggeditOutput.rdata'))
  })
  
  observeEvent(input$exit,{
    rm(list = ls(envir = .GlobalEnv)[ls(envir = .GlobalEnv)%in%c('obj.new','obj.theme','objList.new','obj.Elems','themeUpdate','nonLayers','nonLayersTxt')],envir = .GlobalEnv)
    stopApp(NULL)
  })
  
  output$downloadData <- downloadHandler(
    filename = 'ggeditOutput.rda',
    content = function(file) {
      UpdatedPlots=objList.new
      class(UpdatedPlots)=c("ggedit",class(UpdatedPlots))
      ggeditOut=list(UpdatedPlots=UpdatedPlots,UpdatedLayers=layersListObj(obj = objList.new,lbl=names(objList.new)),UpdatedLayersElements=layersList(objList.new))
      class(ggeditOut)=c("ggedit",class(ggeditOut))
      if(exists('themeUpdate',envir = .GlobalEnv)) ggeditOut$UpdatedThemes=themeUpdate
      save(ggeditOut, file = file)
    }
  )
  
}