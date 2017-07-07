#' @title Creates an independent copy of a ggplot layer object
#' @export
#' @description Creates copies of ggplot layers from within ggplot objects that 
#' are independent of the parent object.
#' @details ggplot objects are comprimsed of layer objects. Once compiled they 
#' are part of the plot object environment and if they are changed internally 
#' regardless of where they are in the (ie different environment) it will change 
#' the original plot. This function allows to create replicates of the plot layers
#' and edit them independent of the original plot. When setting verbose to TRUE 
#' function returns the ggplot2 call as a string to paste in regular ggplot script
#' to generate the layer.
#' @param l ggplot2 object layer
#' @param verbose toggle to control if the output is ggproto object (verbose==FALSE,default) or string of layer call (verbose==TRUE)
#' @param showDefaults toggle to control if the verbose output shows all the input arguments passed to the proto object (if verbose==FALSE then ignored)
#' @return ggproto or string object (conditional on verbose)
#' @examples
#' p=ggplot2::ggplot(iris,ggplot2::aes(x =Sepal.Length,y=Sepal.Width))
#' p=p+ggplot2::geom_point(ggplot2::aes(colour=Species))+ggplot2::geom_line()
#' p$layers[[1]]
#' newLayer=cloneLayer(l=p$layers[[1]])
#' all.equal(p$layers[[1]],newLayer)
#' (v=cloneLayer(l=p$layers[[1]],verbose=TRUE))
#' eval(parse(text=v))
#' all.equal(p$layers[[1]],eval(parse(text=v)))
#'
#' @importFrom utils capture.output
#' @importFrom ggraph get_edges
 
cloneLayer=function(l,verbose=FALSE,showDefaults=TRUE){
  
  if(!exists('ggedit.opts',where = parent.env(environment()))) ggedit.opts<-ggedit::ggedit.opts
  
  parent.layer<-proto_features(l)%>%
    dplyr::left_join(ggedit.opts$geom_opts%>%dplyr::filter_(~!grepl('^stat',fn)), 
                          by = c("position", "geom", "stat"))
  
  
  if(is.na(parent.layer$fn)) parent.layer$fn=paste0(tolower(strsplit(parent.layer$stat,'(?<=Stat)',perl = TRUE)[[1]]),collapse = '_')
  
  layer.names=c('mapping','data','geom','position',
                'stat','show.legend','inherit.aes',
                'aes_params','geom_params','stat_params')
  
  x=sapply(layer.names,function(y){
    b=l[[y]]
    
    if('waiver'%in%class(b)) b=NULL
    
    if(y=='geom') b=eval(parse(text=parent.layer$geom))
    if(y=='position') b=gsub(y, "", tolower(class(b)[1]))
    if(y=='stat') b=eval(parse(text=parent.layer$stat))
    
    # if(y%in%c('position','stat')) {
    #   b=gsub(y, "", tolower(class(b)[1]))
    # }
    
    b
  }) 

  x$params=append(x$stat_params,x$geom_params)
  x$params=append(x$params,x$aes_params)
  x$params=x$params[!duplicated(names(x$params))]
  x$geom_params<-x$aes_params<-x$stat_params<-NULL

  if(typeof(x$data)=='closure'){
    requireNamespace("ggraph")
    x$data<-ggraph::get_edges()
  } 

  if(verbose){
    nm=names(x)
    #nm=nm[!nm%in%c('geom','params','mapping')]
    nm=nm[!sapply(x,typeof)%in%c('environment','closure','list')]
    geom_aes=list(geom   = parent.layer$fn,
                  mapping= paste0(names(x$mapping),sapply(x$mapping,build_map)),
                  params = paste0(names(x$params),sapply(x$params,build_map)),
                  layer  = paste0(rev(nm),sapply(x[rev(nm)],build_map)),
                  data   = paste0('data=',paste0(capture.output(dput(x$data)),collapse='\n'))
    )

    strRet=sprintf('%s(mapping=aes(%s),%s,%s)',
                   paste0(geom_aes$geom,collapse=','),
                   paste0(geom_aes$mapping,collapse=','),
                   paste0(geom_aes$params,collapse=','),
                   paste0(geom_aes$layer,collapse=','),
                   geom_aes$data
                   )
    
    if(!showDefaults){
      
      geom_proto<-cloneProto(eval(parse(text=paste0(geom_aes$geom,'()'))))
      
      geom_diff<-sapply(names(geom_aes)[-1],function(x) geom_aes[[x]][!geom_aes[[x]]%in%geom_proto[[x]]])

      strRet=sprintf('%s(aes(%s),%s,%s,%s)',
                     paste0(geom_aes$geom,collapse=','),
                     paste0(geom_diff$mapping,collapse=','),
                     paste0(geom_diff$params,collapse=','),
                     paste0(geom_diff$layer,collapse=','),
                     geom_aes$data
      )
    }
    strRet=gsub('aes()','',strRet,fixed = T) #failsafe for empty aes() call
    strRet=gsub('[,]{2,}',',',strRet)
    strRet=gsub('data=NULL','',strRet)
    strRet=gsub(',)',')',strRet)
    strRet=gsub('\\(,','(',strRet)

    strRet
  }else{
    do.call(layer,x) 
  }
}