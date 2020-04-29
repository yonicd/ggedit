#' @title class_layer
#' @description Determines the class of the arguments in a gg layer.
#' @param p ggplot
#' @return data.frame
#' @keywords internal
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang quo_name quo_squash
#' @importFrom  plyr ddply
#' 
class_layer <- function(p){

  if( 'tbl'%in%class(p$data) ) 
    p$data <- data.frame(p$data)

  plot_aes = layer_aes = NULL
  
  if( length(as.character(p$mapping))>0 ){
    
  plot_aes <- sapply(p$mapping,rlang::quo_name)
  
  aes_nm <- names(p$mapping)[plot_aes!='NULL']
  
  plot_aes <- plot_aes[plot_aes!='NULL']
  
  plot_aes <- data.frame(VAR=unlist(plot_aes),stringsAsFactors = FALSE)
  
  plot_aes$aes <- aes_nm
  
  plot_cl <- lapply(plot_aes$aes,function(x){
    
    TEMP <- p$data%>%
      dplyr::mutate(.NEWVAR = !!rlang::quo_squash(p$mapping[[x]]))
    
    class(TEMP[['.NEWVAR']])

    })
    
  chkIdx <- which(unlist(lapply(plot_cl,length))!=1)
  
  for( i in chkIdx ){
    
    if( 'data.frame'%in%plot_cl[[i]] ){
      
      plot_cl[[i]]='data.frame'
      
    }else{
      
      break(sprintf('classes:c(%s) for plot variable:%s. Please choose one and rerun',paste0(plot_cl[[i]],collapse=','), plot_aes$VAR[i]))
      
    }
    
  }
  
  plot_aes$class <- unlist(plot_cl)[1]
  
  plot_aes$layer <- 'plot'
  
  }
  
  layer_aes <- lapply(p$layers,function(x){
    mappings <- x$mapping
    data.frame(VAR = if(is.null(mappings)) character() else sapply(mappings, quo_name),
               aes = if(is.null(mappings)) character() else names(x$mapping),stringsAsFactors = FALSE) %>%
      dplyr::filter(!is.null(!!rlang::sym('VAR')))
    
  })
  
  layer_data <- lapply(p$layers,function(x){
    
             dOut <- x$data
             
             if('tbl'%in%class(dOut)) 
               dOut <- data.frame(dOut,stringsAsFactors = FALSE)
             
             dOut
             
  })

  pData <- lapply(p$layers,function(l,d) l$layer_data(d), p$data)
    
  names(pData) <- names(layer_aes) <- names(layer_data) <- geom_list(p)
  
  pData$plot <- p$data
  
  layer_aes <- dplyr::bind_rows(layer_aes,.id = 'layer')

  layer_aes <- plyr::ddply(layer_aes,.variables = c('layer','VAR','aes'),.fun=function(df){

      TEMP <- pData[[df$layer]]%>%
        dplyr::mutate(.NEWVAR = eval(parse(text = df$VAR)))
    
      df$class <-class(TEMP[['.NEWVAR']])
    
    df
  })
  
  layer_bind <- rbind(plot_aes,layer_aes)
  
  layer_bind <- plyr::ddply(layer_bind,
                            .variables = c('layer','VAR','aes'),
                            .fun=function(df){
    
        TEMP <- pData[[df$layer]]%>%
          dplyr::mutate(.NEWVAR = eval(parse(text = df$VAR)))
        
        df$level.num <- length(unique(TEMP[,'.NEWVAR']))
                              
    df
  })
  
  return(layer_bind)
}