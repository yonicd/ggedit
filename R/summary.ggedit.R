#'@title Print to console verbose outputs of objects of class ggedit
#'@description function to tidy the ggedit output to single script calls for each plot
#'@param object ggedit object
#'@param ... used to pass show.structure
#'@details use show.structure (boolean) to control if the output shows the full data.frame
#' structure or just a placeholder [data.frame], Default: FALSE
#'@export
summary.ggedit=function(object, ...){
if(!inherits(object,'ggedit')) stop('Not ggedit object')
  show.structure <- FALSE
  
  list2env(list(...),envir = environment())
  
  out<-dput.ggedit(object)

  if(!show.structure) out<-gsub(',data=structure(.*?)class = "data.frame"\\)',',\\[data.frame\\]',out)
  
 writeLines(paste0('\n',names(out),'\n',out))
 
}


