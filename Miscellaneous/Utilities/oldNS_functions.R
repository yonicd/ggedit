
#' @title aesColorNS
#' @description ColorInput UI production for discrete variables for module.
#' @param type character of label and inputId of element
#' @param session shiny session object
#' @return UI object
aesColourNS=function(type,session) {
  ns<-session$ns
  list(type=colourpicker::colourInput,
       args=list(inputId = ns(paste0('pop',toupper(type))),
                 label = type,
                 value = NA,
                 allowTransparent = T)
  )
}


#' @title aesColorContNS
#' @description ColorInput UI production for continuous variables for module.
#' @param type character of label and inputId of element
#' @param session shiny session object
#' @return UI object
#' @import shiny
#' @keywords internal
aesColourContNS=function(type,session) {
  ns<-session$ns
  id=gsub("-a", "", ns("a"))
  iId=paste0('pop',toupper(type),'fixedPal')
  fP=ns(paste0('pop',toupper(type),'fixedPal'))
  #column(width=2,
  shiny::div(
    shiny::selectizeInput(inputId = fP,
                          label='Pallete',
                          choices = c('Manual','Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges', 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd'),
                          selected = 'Blues'),
    
    shiny::conditionalPanel(paste0("input['", id, "-", iId ,"'] == 'Manual'"),
                            lapply(c('Low','High'),function(x,type){
                              if(x=='Low'){
                                pad='padding:0px 0px 0px 10px;'
                                init.col='red'
                              } 
                              if(x=='High'){
                                pad='padding:0px 10px 0px 0px;'
                                init.col='blue'
                              } 
                              
                              shiny::column(width=6, style=pad,
                                            do.call(colourpicker::colourInput,
                                                    args=list(inputId = ns(paste0('pop',toupper(type),x)),
                                                              label = x,
                                                              value =  init.col,
                                                              returnName = F,
                                                              showColour = "background")
                                            )
                              )
                            },type)
    )
  )
}

#' @title aesSelectNS
#' @description SelectizeInput UI production for palletes for module.
#' @param type character of label and inputId of element
#' @param session shiny session object
#' @return UI object
#' @import shiny
#' @keywords internal
aesSelectNS=function(type,session) {
  ns<-session$ns
  list(type=shiny::selectizeInput,
       args=list(options=list(plugins=list('drag_drop','remove_button')),
                 inputId = ns(paste0('pop',toupper(type))),
                 label = type,
                 choices=c(scales::hue_pal()(10),NA),selected=NA)
  )
}

#' @title aesSlideNS
#' @description sliderInput UI production for continuous variables for module
#' @param type character of label and inputId of element
#' @param session shiny session object
#' @return UI object
#' @import shiny
#' @keywords internal
aesSlideNS=function(type,session){
  ns<-session$ns
  
  slideDefaults <- ggedit_opts$get('slideDefaults')
  
  list(type=shiny::sliderInput,
       args=list(inputId = ns(paste0('pop',toupper(type))),
                 label = type,
                 min = slideDefaults[[type]][1],
                 max = slideDefaults[[type]][2],
                 value = NA)
  )
}

#' @title arg.valueNS
#' @description Creates list of arguments and values in gg layer for module.
#' @param item character type of gg layer
#' @param obj.elems elements in layer
#' @param session shiny session object
#' @return list
#' @keywords internal
arg.valueNS=function(item,obj.elems,session){
  ns<-session$ns
  item_class=obj.elems[[item]]$class[[1]]
  if(item_class=='data.frame'){
    if(item%in%c('colour','color','fill')){
      x=aesColourNS(item,session)
      y=obj.elems[[item]]$val[[1]]
      if(!grepl("[#]",y)) y=scales::col2hcl(y)
      x$args$value=closestColHex(y)[1]
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
        x=list(x=x)
      }else{
        stop("non colour aesthetics of numeric inputs are not currently supported in ggedit", call. = FALSE)
      }
    }
    if(item_class%in%c('character','factor')){
      x=lapply(obj.elems[[item]]$val[[1]],function(y){
        if(item%in%c('colour','color','fill')){
          z=aesColourNS(item,session)
          if(!grepl("[#]",y)) y=scales::col2hcl(y)
          z$args$value=closestColHex(y)[1]
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

#' @title themeMakePanel
#' @description BS modal production for the gg theme, for module.
#' @param x list from themeFetch
#' @param session shiny session object
#' @return UI
#' @keywords internal
themeMakePanelNS=function(x,y=NULL,session){
  if(any(names(x[[1]])=='call')){
    if(is.null(y)) y=names(x)
    out=themePanelNS(x,y,session)
  }else{
    a=lapply(names(x[[1]]),FUN = function(y) {
      themeMakePanelNS(x=x[[1]][y],y=names(x),session=session)
    })
    out=shiny::tabPanel(title=names(x),do.call(what = shiny::tabsetPanel,args = unlist(a,recursive = F)))
  }
  return(out)
}

#' @title themePanelNS
#' @description UI production for the gg theme, for modal.
#' @param obj.in theme object in list form
#' @param obj.name name of theme object
#' @param session shiny session object
#' @return UI
#' @import shiny
#' @keywords internal
themePanelNS=function(obj.in,obj.name,session){
  ns<-session$ns
  
  themeTips <- ggedit_opts$get('themeTips')
  
  lapply(names(obj.in) ,FUN = function(item){
    do.call(what = shiny::tabPanel,
            args = list(title=item,
                        shiny::fluidRow(lapply(utils::head(names(obj.in[[item]]),-1),
                                               FUN=function(subitem){
                                                 x=obj.in[[item]][[subitem]]
                                                 txt.in=unique(c(obj.name,item,subitem))
                                                 if(obj.name=="legend"&item%in%c("position","justification")) txt.in=c(obj.name,item,subitem)
                                                 obj=shiny::column(width=3,shiny::textInput(inputId = ns(paste0(c("pop",txt.in),collapse="")),
                                                                                            label = x['name'],
                                                                                            value = x['value'])
                                                 )
                                                 return(obj)
                                               })),
                        shiny::HTML(paste0(
                          paste(paste0('<b>',names(themeTips[[obj.in[[item]]$call]]),'</b>'),
                                unlist(themeTips[[obj.in[[item]]$call]]),
                                sep=': '),
                          sep="<br/>"))
            )
    )
  }
  )
}