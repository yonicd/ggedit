#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom scales brewer_pal linetype_pal
#' @importFrom shinyAce aceEditor
#' @importFrom shinyBS bsModal
#' @importFrom utils capture.output
#' @importFrom graphics plot
ggeditGadget <- function(viewer=shiny::paneViewer(minHeight = 1000),...) {
  TEMPLIST<-new.env()
  TEMPLIST$obj<-get(".p", envir = .ggeditEnv)

  verbose<- get(".verbose", envir = .ggeditEnv)  
  showDefaults<- get(".showDefaults", envir = .ggeditEnv)  
  plotWidth<- get(".plotWidth", envir = .ggeditEnv)  
  plotHeight<- get(".plotHeight", envir = .ggeditEnv)
  
    ui <-miniUI::miniPage( 
      
      miniUI::gadgetTitleBar("Edit ggplots themes and layer aesthetics"),
      miniUI::miniContentPanel(
        shiny::fluidPage(
          shiny::div(class='row',
                 shiny::column(width=3,shiny::actionLink("updateElem","Update Plot Layer")),
                 shiny::column(width=2,shiny::actionLink("updateTheme","Update Plot Theme")),
                 shiny::column(width=2,shiny::actionLink("SetThemeGrid",'Update Grid Theme')),
                 shiny::column(width=3,shiny::actionLink("SetThemeGlobal",'Update Global Theme')),
                 shiny::column(width=2,shiny::actionLink('viewVerbose','View Layer Code'))
        ),
        shiny::hr(),
        shiny::conditionalPanel('input.viewVerbose',shiny::uiOutput("SimPrint")),
        shiny::column(width=3,shiny::selectInput("activePlot","Choose Plot:",choices = split(1:length(TEMPLIST$obj),factor(names(TEMPLIST$obj),levels=names(TEMPLIST$obj),ordered=T)), selected = 1)),
        shiny::column(width=6,shiny::uiOutput('layers')),
        shiny::plotOutput(outputId = "Plot",height = "300px"),
        shiny::uiOutput('popElems'),
        shiny::uiOutput('popTheme')
      )
      )
    )

    server = function(input, output, session) {
        #Plots----
        TEMPLIST$objList.new<- TEMPLIST$obj
        TEMPLIST$nonLayers<-vector('list',length(TEMPLIST$objList.new))
        TEMPLIST$nonLayersTxt<-vector('list',length(TEMPLIST$objList.new))
        
        #populate select with the plots chosen to work on
        output$activePlot=shiny::renderUI({
          
          if(is.null(session)){
            ns <- function(x) x 
          }else{
            ns <- session$ns  
          }
          
          nm <- factor(names(TEMPLIST$objList.new),ordered = T,levels=names(TEMPLIST$objList.new))
          
          shiny::selectInput(ns("activePlot"),"Choose Plot:",choices = split(1:length(nm),nm),selected = 1)
        })
        
        baseLayerVerbose=lapply(TEMPLIST$obj,function(x) lapply(x$layers,function(y) cloneLayer(y,verbose = T,showDefaults = showDefaults)))
        
        plotIdx=shiny::reactive({
          if(is.null(input$activePlot)){
            1
          }else{
            as.numeric(input$activePlot)
          }
        })

        shiny::observe(TEMPLIST$obj.new<-TEMPLIST$objList.new[[plotIdx()]])

        theme.now=ggplot2::theme_get()
        TEMPLIST$obj.theme<-lapply(TEMPLIST$objList.new,function(p){
          if(length(p$theme)>0) theme.now=theme.now%+replace%p$theme
          themeFetch(theme.now)
        })

#Layers----
        output$layers=shiny::renderUI({
          
          if(is.null(session)){
            ns <- function(x) x 
          }else{
            ns <- session$ns  
          }
          
          TEMPLIST$obj.new<-TEMPLIST$objList.new[[as.numeric(input$activePlot)]]
          shiny::radioButtons(ns("geoms"),"Choose layer(s):",choices = geom_list(TEMPLIST$obj.new),selected = geom_list(TEMPLIST$obj.new)[1],inline = T)
        })

        update.Layer=shiny::eventReactive(input$sendElem,{
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
                  palTxt=paste0("scale_",item,"_gradientn(colours=scales::brewer_pal(palette=",palItem,",direction=-1)(9)[1:5])")
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
                if(!grepl('size|shape|linetype',item)) vals=paste0("'",vals,"'")
                if(grepl('linetype',item)){
                  vals=match(vals,c('0',scales::linetype_pal()(6)))-1
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

        output$popElems=shiny::renderUI({
          
          if(is.null(session)){
            ns <- function(x) x 
          }else{
            ns <- session$ns  
          }
          
          TEMPLIST$obj.new<-TEMPLIST$objList.new[[as.numeric(input$activePlot)]]
          TEMPLIST$obj.Elems<-fetch_aes_ggplotBuild(TEMPLIST$obj.new,geom_list(TEMPLIST$obj.new))
          if(is.null(input$geoms)){
            gIdx=1
          }else{
            gIdx=input$geoms
            }
          obj.elems=TEMPLIST$obj.Elems[[gIdx]]
          obj.elems=obj.elems[!names(obj.elems)%in%c('family','lineend')]
          obj.elemsL=list()
          for(item in names(obj.elems)){
              item_class=obj.elems[[item]]$class[[1]]
              if(grepl('colour|fill|color',item)){
                divName='divColor'
                if(is.null(obj.elemsL[[divName]])) obj.elemsL[[divName]]=list()
              }else{
                if(item_class=='data.frame'){
                  divName='divSlide'
                  if(is.null(obj.elemsL[[divName]])) obj.elemsL[[divName]]=list()
                }
                if(item_class%in%c('character','factor')){
                  divName='divSelect' 
                  if(is.null(obj.elemsL[[divName]])) obj.elemsL[[divName]]=list()
                }
              }
              obj.elemsL[[divName]][[item]]=obj.elems[[item]]
          }

          shinyBS::bsModal(id = ns("updateElemPopup"), title = "Update Plot Layer", trigger = ns("updateElem"), size = "large",
                           shiny::fluidRow(
                    lapply(obj.elemsL,function(objItem){
                      shiny::column(4,
                           lapply(names(objItem) ,FUN = function(item){
                             list(
                               lapply(arg.value(item, objItem, session),function(x) {
                                 do.call(what = x[['type']],args = x[['args']])
                               })
                             )
                           })
                           )
                      })
                    ),
                    shiny::div(align="right",shiny::actionButton(ns("sendElem"),"Update Layer"))
          )
        })
#Theme----
        update.Theme=shiny::eventReactive(input$sendTheme,{
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

        shiny::observeEvent(input$SetThemeGlobal,{
          if(length(TEMPLIST$obj.new$theme)>0) theme.now=theme.now+TEMPLIST$obj.new$theme
          ggplot2::theme_set(ggplot2::theme_get()%+replace%theme.now)
        })

        update.ThemeGrid=shiny::eventReactive(input$SetThemeGrid,{
          TEMPLIST$p.now<-TEMPLIST$objList.new[[as.numeric(input$activePlot)]]
          if(length(TEMPLIST$p.now$theme)>0) theme.now=theme.now+TEMPLIST$p.now$theme

          for(i in 1:length(TEMPLIST$objList.new)){
            TEMPLIST$objList.new[[i]]<- TEMPLIST$objList.new[[i]]+theme.now
            TEMPLIST$themeUpdate[[i]]<- TEMPLIST$objList.new[[i]]$theme
          }

          return(TEMPLIST$objList.new)
        })

        output$popTheme=shiny::renderUI({
          
          if(is.null(session)){
            ns <- function(x) x 
          }else{
            ns <- session$ns  
          }
          
          shinyBS::bsModal(id = ns("updateThemePopup"), title = shiny::HTML('Update Plot Theme <a href="http://docs.ggplot2.org/0.9.3.1/theme.html" target="_blank">(help)</a>'), trigger = ns("updateTheme"), size = "large",

                  do.call(shiny::tabsetPanel,

                          unlist(lapply(1:length(TEMPLIST$obj.theme[[plotIdx()]]),FUN = function(j){
                            if(themeListDepth(TEMPLIST$obj.theme[[plotIdx()]][j])>2){
                              list(themeMakePanel(TEMPLIST$obj.theme[[plotIdx()]][j],session=session))
                            }else{
                              unlist(lapply(j, function(i) {
                                themeMakePanel(TEMPLIST$obj.theme[[plotIdx()]][i],session=session)
                                }),F)}
                          }),F)


                  ),
                  shiny::hr(),
                  shiny::div(align="right",shiny::actionButton(ns("sendTheme"),"Set Theme"))


          )
        })

#Render Plot----
        output$Plot=shiny::renderPlot({
          as.ggedit(TEMPLIST$objList.new)
        },width=plotWidth,height=plotHeight)

        shiny::observeEvent(input$updateElem,{
          output$Plot=shiny::renderPlot({
            if(input$sendElem==0){
              as.ggedit(TEMPLIST$objList.new)
            }else{
              pList.out=update.Layer()
              as.ggedit(pList.out)
            }
          },width=plotWidth,height=plotHeight)
        })

        shiny::observeEvent(input$updateTheme,{
          output$Plot=shiny::renderPlot({
            if(input$sendTheme==0){
              as.ggedit(TEMPLIST$objList.new)
            }else{
              pList.out=update.Theme()
              as.ggedit(pList.out)
            }
          },width=plotWidth,height=plotHeight)
        })

        shiny::observeEvent(input$SetThemeGrid,{
          pList.out=update.ThemeGrid()
          output$Plot=shiny::renderPlot({as.ggedit(pList.out)},width=plotWidth,height=plotHeight)
        })

        shiny::observeEvent(input$done, {
          UpdatedPlots=as.ggedit(TEMPLIST$objList.new)
          class(UpdatedPlots)=c("ggedit",class(UpdatedPlots))
          
          ggeditOut=list(UpdatedPlots=UpdatedPlots,
                         UpdatedLayers=layersListObj(obj = TEMPLIST$objList.new,lbl=names(TEMPLIST$objList.new)),
                         UpdatedLayersElements=layersList(TEMPLIST$objList.new)
                         )

          if(verbose) ggeditOut$UpdatedLayerCalls=lapply(TEMPLIST$objList.new,function(p) lapply(p$layer,function(item) cloneLayer(l = item,verbose = T,showDefaults = showDefaults)))
                    
          names(TEMPLIST$nonLayers)<-names(TEMPLIST$nonLayersTxt)<-names(TEMPLIST$objList.new)
          ggeditOut$updatedScales=TEMPLIST$nonLayers

          if(verbose) ggeditOut$UpdatedScalesCalls=TEMPLIST$nonLayersTxt            
          
          
          if(exists('themeUpdate',envir = TEMPLIST)) {
            ggeditOut$UpdatedThemes=TEMPLIST$themeUpdate
            if(verbose){
              ggeditOut$UpdatedThemeCalls=lapply(names(TEMPLIST$objList.new),function(lp,input){
                p=TEMPLIST$objList.new[[lp]]
                if(length(p$theme)>0){
                  if(!showDefaults){
                    themeBase=ggplot2::theme_get()
                    if(length(TEMPLIST$obj[[lp]]$theme)>0) themeBase=themeBase+TEMPLIST$obj[[lp]]$theme
                    compare(p$theme,themeBase,verbose=T)
                  }else{
                    x.theme=themeFetch(p$theme)
                    x=lapply(names(x.theme),function(item){themeNewVal(x.theme[item],p,input)})
                    paste0("theme(",paste0(unlist(x),collapse = ","),")")
                  }

                }else{
                c('list()')
                }
              },input)
              names(ggeditOut$UpdatedThemeCalls)=names(TEMPLIST$objList.new)
            } 
            }
          
          class(ggeditOut)=c("ggedit",class(ggeditOut))
          
          #rm(list = ls(envir = .GlobalEnv)[ls(envir = .GlobalEnv)%in%c('obj.new','obj.theme','objList.new','obj.Elems','themeUpdate','nonLayers','nonLayersTxt')],envir = .GlobalEnv)
          shiny::stopApp(ggeditOut)
        })
        
        shiny::observeEvent(input$cancel,{
          #rm(list = ls(envir = .GlobalEnv)[ls(envir = .GlobalEnv)%in%c('obj.new','obj.theme','objList.new','obj.Elems','themeUpdate','nonLayers','nonLayersTxt')],envir = .GlobalEnv)
          shiny::stopApp(NULL)
        })
        
        simTxt=shiny::reactive({
          LayerVerbose<-lapply(TEMPLIST$objList.new,function(p) lapply(p$layer,function(item) cloneLayer(l = item,verbose = T,showDefaults = showDefaults)))
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
          
          if(length(baseLayerVerbose)>0) strBase=baseLayerVerbose[[aP]][[l]]
          return(list(Original=strBase,Edited=strNew))
        })
        
        output$SimPrint <- shiny::renderUI({
          
          if(is.null(session)){
            ns <- function(x) x 
          }else{
            ns <- session$ns  
          }
          
          junk=''
          if(length(simTxt())>0) junk=textConnection(utils::capture.output(simTxt()))
          toace=paste0(readLines(junk),collapse='\n')
          if(input$viewVerbose%%2==1){
            if (Sys.info()[1] == "Windows"){
              output$codeout<-shiny::renderText({toace})  
              shiny::verbatimTextOutput('codeout')
            }else{
              shinyAce::aceEditor(outputId = "codeout",value=toace,mode = "r", theme = "chrome", height = "100px", fontSize = 12) 
            }
          }
        })
        
        
    }

    shiny::runGadget(ui, server, stopOnCancel = FALSE, viewer = viewer)
  

}
