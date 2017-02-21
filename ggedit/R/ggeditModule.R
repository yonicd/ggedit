#' @export
#' @keywords internal
ggEdit<- function(input, output, session,obj) {
  verbose=T
  TEMPLIST<-new.env()

  observe({
    TEMPLIST$objList.new<-list() 
    p.in<-obj()
    if(is.ggplot(p.in)) p.in=list(p.in)
    
    if(is.null(names(p.in))) names(p.in)=as.character(1:length(p.in))
    
    if(!all(unlist(lapply(p.in,is.ggplot)))) stop("'object' is not a valid ggplot object")
    
    TEMPLIST$objList.new <- p.in
    TEMPLIST$obj.theme<-vector('list',length(TEMPLIST$objList.new))
    TEMPLIST$nonLayers<-vector('list',length(TEMPLIST$objList.new))
    TEMPLIST$nonLayersTxt<-vector('list',length(TEMPLIST$objList.new))
    })
  
  
  #populate select with the plots chosen to work on
  output$activePlot=renderUI({
    ns<-session$ns
    nm=factor(names(TEMPLIST$objList.new),ordered = T,levels=names(TEMPLIST$objList.new))
    
    
    selectInput(ns("activePlot"),"Choose Plot:",choices = split(1:length(nm),nm),selected = 1)
  })
  
  baseLayerVerbose<-eventReactive(input$activePlot,{
    p.in<-obj()
    if(is.ggplot(p.in)) p.in=list(p.in)
    
    if(is.null(names(p.in))) names(p.in)=as.character(1:length(p.in))
    
    lapply(p.in,function(x) lapply(x$layers,function(y) cloneLayer(y,verbose = T)))
  })
  
  plotIdx=eventReactive(input$activePlot,{
    if(is.null(input$activePlot)){
      1
    }else{
      as.numeric(input$activePlot)
    }
  })
  
  observe(TEMPLIST$obj.new<-TEMPLIST$objList.new[[plotIdx()]])
  
  theme.now=theme_get()
  
  
  #Layers----
  observeEvent(input$activePlot,{
    output$layers=renderUI({
      ns<-session$ns
      radioButtons(ns("geoms"),"Choose layer(s):",choices = geom_list(TEMPLIST$obj.new),selected = geom_list(TEMPLIST$obj.new)[1],inline = T)
    })
    
    TEMPLIST$obj.theme<-lapply(TEMPLIST$objList.new,function(p){
      if(length(p$theme)>0) theme.now=theme.now+p$theme
      themeFetch(theme.now)
    })
    
  })
  
  update.Layer=eventReactive(input$sendElem,{
    TEMPLIST$obj.new<-TEMPLIST$objList.new[[as.numeric(input$activePlot)]]
    layer.idx=which(geom_list(TEMPLIST$obj.new)==input$geoms)
    numElem=unlist(lapply(TEMPLIST$obj.Elems[[layer.idx]],function(x) length(x$val[[1]])))
    for(item in names(TEMPLIST$obj.Elems[[layer.idx]])){
      if(numElem[item]==1) {
        newLayer=cloneLayer(TEMPLIST$obj.new$layers[[layer.idx]])
        newLayer$aes_params[[item]]=eval(parse(text=paste0('input$pop',toupper(item))))
        TEMPLIST$obj.new$layers[[layer.idx]]<-newLayer
      }else{
        if(TEMPLIST$obj.Elems[[layer.idx]][[item]][['class']][[1]]=='numeric'){
          if(input[[paste0('pop',toupper(item),'fixedPal')]]!='Manual'){
            palItem=paste0("'",input[[paste0('pop',toupper(item),'fixedPal')]],"'")
            palTxt=paste0("scale_",item,"_gradientn(colours=brewer_pal(palette=",palItem,",direction=-1)(9)[1:5])")
            TEMPLIST$nonLayersTxt[[as.numeric(input$activePlot)]][[paste0("scale_",item,"_gradientn")]]<-palTxt
            suppressMessages({nL=eval(parse(text=palTxt))})
            TEMPLIST$nonLayers[[as.numeric(input$activePlot)]][[paste0("scale_",item,"_gradientn")]]<-nL
            suppressMessages({eval(parse(text=paste0("TEMPLIST$obj.new<-TEMPLIST$obj.new+",palTxt)))})
          }else{
            LowCol=paste0("'",input[[paste0('pop',input$pop,toupper(item),'Low')]],"'")
            HighCol=paste0("'",input[[paste0('pop',input$pop,toupper(item),'High')]],"'")
            ColTxt=paste0("scale_",item,"_gradient(low=",LowCol,",high=",HighCol,")")                  
            TEMPLIST$nonLayersTxt[[as.numeric(input$activePlot)]][[paste0("scale_",item,"_gradient")]]<-ColTxt
            suppressMessages({nL=eval(parse(text=ColTxt))})
            TEMPLIST$nonLayers[[as.numeric(input$activePlot)]][[paste0("scale_",item,"_gradient")]]<-nL
            suppressMessages({eval(parse(text=paste0("TEMPLIST$obj.new<-TEMPLIST$obj.new+",ColTxt)))})
          }
        }else{
          vals=unlist(lapply(names(input)[grepl(paste0('pop',toupper(item),'[1-9]'),names(input))],function(x) input[[x]]))
          if(!item%in%c('size','shape','linetype')) vals=paste0("'",vals,"'")
          if(item=='linetype') {
            vals=match(vals,c('0',linetype_pal()(6)))-1
          }
          
          vals=paste0(vals,collapse=',')
          TEMPLIST$nonLayersTxt[[as.numeric(input$activePlot)]][[paste0("scale_",item,"_manual")]]<-paste0("scale_",item,"_manual(values=c(",vals,"))")
          suppressMessages({nL=eval(parse(text=paste0("scale_",item,"_manual(values=c(",vals,"))")))})
          TEMPLIST$nonLayers[[as.numeric(input$activePlot)]][[paste0("scale_",item,"_manual")]]<-nL
          suppressMessages(eval(parse(text=paste0("TEMPLIST$obj.new<-TEMPLIST$obj.new+scale_",item,"_manual(values=c(",vals,"))"))))
        }
      }
    }
    TEMPLIST$objList.new[[as.numeric(input$activePlot)]]<-TEMPLIST$obj.new
    return(TEMPLIST$objList.new)
  })
  
  output$popElems=renderUI({
    ns<-session$ns
    if(is.null(input$activePlot)){
      aP=1
    }else{
      aP=as.numeric(input$activePlot)
    } 
    TEMPLIST$obj.new<-TEMPLIST$objList.new[[aP]]
    TEMPLIST$obj.Elems<-fetch_aes_ggplotBuild(TEMPLIST$obj.new,geom_list(TEMPLIST$obj.new))
    if(is.null(input$geoms)){
      gIdx=1
    }else{
      gIdx=input$geoms
    }
    obj.elems=TEMPLIST$obj.Elems[[gIdx]]
    
    arg.value=function(item){
      item_class=obj.elems[[item]]$class[[1]]
      if(item_class=='data.frame'){
        if(item%in%c('colour','color','fill')){
          x=aesColourNS(item,session)
          y=obj.elems[[item]]$val[[1]]
          if(!grepl("[#]",y)) y=col2hcl(y)
          x$args$value=colourpicker:::closestColHex(y)[1]
          x=list(x=x)
        }else{
          x=aesSlideNS(item,session)
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
            x[['type']]=aesColourContNS
            x$args=list(type=item,session=session)
            # x$args$selected='Blues'
            # x$args$choices=c(NA,'Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges', 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd')
            x=list(x=x)
          }else{
            stop("non colour aesthetics of numeric inputs are not currently supported in ggedit", call. = FALSE)
          }
        }
        if(item_class%in%c('character','factor')){
          x=lapply(obj.elems[[item]]$val[[1]],function(y){
            if(item%in%c('colour','color','fill')){
              z=aesColourNS(item,session)
              if(!grepl("[#]",y)) y=col2hcl(y)
              z$args$value=colourpicker:::closestColHex(y)[1]
            }else{
              z=aesSelectNS(item,session)
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
            x[[i]]$args$inputId=ns(paste0(x[[i]]$args$inputId,i))
          }
        }
      }
      
      return(x)
    }
    
    bsModal(id = ns("updateElemPopup"), title = "Update Plot Layer", trigger = ns("updateElem"), size = "large",
            
            fluidRow(
              lapply(names(obj.elems)[!names(obj.elems)%in%c('family')] ,FUN = function(item){
                list(
                  column(width = 3,
                         lapply(arg.value(item),function(x) {
                           do.call(what = x[['type']],args = x[['args']])
                         }))
                )
              })),
            div(align="right",actionButton(ns("sendElem"),"Update Layer"))
            
            
            
            
    )
  })
  #Theme----
  update.Theme=eventReactive(input$sendTheme,{
    TEMPLIST$obj.new<-TEMPLIST$objList.new[[as.numeric(input$activePlot)]]
    
    strThemeCallList=lapply(names(TEMPLIST$obj.theme[[plotIdx()]]),function(item){
      themeNewVal(TEMPLIST$obj.theme[[plotIdx()]][item],TEMPLIST$obj.new,input)
    })
    
    strThemeCall=paste0("TEMPLIST$obj.new<-TEMPLIST$obj.new+theme(",paste0(unlist(strThemeCallList),collapse = ","),")")
    
    eval(parse(text=strThemeCall))
    TEMPLIST$objList.new[[as.numeric(input$activePlot)]]<-TEMPLIST$obj.new
    
    TEMPLIST$themeUpdate<-lapply(TEMPLIST$objList.new,function(p) p$theme)
    return(TEMPLIST$objList.new)
  })
  
  observeEvent(input$SetThemeGlobal,{
    if(length(TEMPLIST$obj.new$theme)>0) theme.now=theme.now+TEMPLIST$obj.new$theme
    theme_set(theme_get()%+replace%theme.now)
  })
  
  update.ThemeGrid=eventReactive(input$SetThemeGrid,{
    p.now<-TEMPLIST$objList.new[[as.numeric(input$activePlot)]]
    if(length(p.now$theme)>0) theme.now=theme.now+p.now$theme
    
    for(i in 1:length(TEMPLIST$objList.new)) TEMPLIST$objList.new[[i]]<- TEMPLIST$objList.new[[i]]+theme.now
    
    return(TEMPLIST$objList.new)
  })
  
  observeEvent(input$activePlot,{
    output$popTheme=renderUI({
      ns<-session$ns
      bsModal(id = ns("updateThemePopup"), title = HTML('Update Plot Theme <a href="http://docs.ggplot2.org/0.9.3.1/theme.html" target="_blank">(help)</a>'), trigger = ns("updateTheme"), size = "large",
              
              do.call(tabsetPanel,
                      unlist(lapply(1:length(TEMPLIST$obj.theme[[plotIdx()]]),FUN = function(j){
                        if(themeListDepth(TEMPLIST$obj.theme[[plotIdx()]][j])>2){
                          list(themeMakePanelNS(TEMPLIST$obj.theme[[plotIdx()]][j],session=session))
                        }else{
                          unlist(lapply(j, function(i) {themeMakePanelNS(TEMPLIST$obj.theme[[plotIdx()]][i],session=session)}),F)}
                      }),F)
                      
                      
              ),
              hr(),
              div(align="right",actionButton(ns("sendTheme"),"Set Theme"))
              
              
      )
    })
    
  })
  
  #Render Plot----
  output$Plot=renderPlot({
    plot(as.ggedit(TEMPLIST$objList.new))
  })
  
  observeEvent(input$updateElem,{
    output$Plot=renderPlot({
      if(input$sendElem==0){
        plot(as.ggedit(TEMPLIST$objList.new))
      }else{
        pList.out=update.Layer()
        plot(as.ggedit(pList.out))
      }
    })
  })
  
  observeEvent(input$updateTheme,{
    output$Plot=renderPlot({
      if(input$sendTheme==0){
        plot(as.ggedit(TEMPLIST$objList.new))
      }else{
        pList.out=update.Theme()
        plot(as.ggedit(pList.out))
      }
    })
  })
  
  observeEvent(input$SetThemeGrid,{
    pList.out=update.ThemeGrid()
    output$Plot=renderPlot({plot(as.ggedit(pList.out))})
  })
  
  simTxt=reactive({
    LayerVerbose<-lapply(TEMPLIST$objList.new,function(p) lapply(p$layer,function(item) cloneLayer(l = item,verbose = T)))
    if(is.null(input$activePlot)){
      aP=1
    }else{
      aP=as.numeric(input$activePlot)
    } 
    
    if(is.null(input$geoms)){
      l=1
    }else{
      l=which(geom_list(TEMPLIST$obj.new)==input$geoms)
    }
    
    a=input$updateElem
    a1=input$updateElemPopup
    
    if(length(l)==0) l=1
    strNew=strBase=''
    if(length(LayerVerbose)>0) strNew=LayerVerbose[[aP]][[l]]
    if(length(baseLayerVerbose())>0) strBase=baseLayerVerbose()[[aP]][[l]]
    return(list(Original=strBase,Edited=strNew))
  })
  
  output$SimPrint <- renderUI({
    ns<-session$ns
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
  
  Out<-eventReactive({c(input$sendTheme,input$sendElem)},{
    if(!is.null(input$sendTheme)){
      if(input$sendTheme>0) junk1<-update.Theme()  
    }
    
    if(!is.null(input$sendElem)){
      if(input$sendElem>0) junk1<-update.Layer()  
    }
 
    #lOut=reactiveValuesToList(input)
    #save(lOut,file="/Users/jonathans/projects/svn-user-yonis/ggedit/module/lOut.rda")  
          
  ggeditOut=list()
  ggeditOut$UpdatedPlots=TEMPLIST$objList.new
  class(ggeditOut$UpdatedPlots)=c("ggedit",class(ggeditOut$UpdatedPlots))
  ggeditOut$UpdatedLayers=layersListObj(obj = TEMPLIST$objList.new,lbl=names(TEMPLIST$objList.new))
  ggeditOut$UpdatedLayersElements=layersList(TEMPLIST$objList.new)
  

  if(verbose) ggeditOut$UpdatedLayerCalls=lapply(TEMPLIST$objList.new,function(p) lapply(p$layer,function(item) cloneLayer(l = item,verbose = T)))

  names(TEMPLIST$nonLayers)<-names(TEMPLIST$nonLayersTxt)<-names(TEMPLIST$objList.new)
  ggeditOut$updatedScales=TEMPLIST$nonLayers

  if(verbose) ggeditOut$UpdatedScalesCalls=TEMPLIST$nonLayersTxt

  
  if('themeUpdate'%in%names(TEMPLIST)) {
    
    ggeditOut$UpdatedThemes=TEMPLIST$themeUpdate
    if(verbose){
      ggeditOut$UpdatedThemeCalls=lapply(TEMPLIST$objList.new,function(p,input){
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
  return(ggeditOut)
  })
  return(Out)
}