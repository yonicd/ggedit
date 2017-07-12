#'@title Print to console verbose outputs of objects of class ggedit
#'@description function to tidy the ggedit output to single script calls for each plot
#'@param x ggedit object
#'@param show.structure boolean, controls if the output shows the full data.frame structure or just a placeholder [data.frame], Default: FALSE
#'@param ... nothing
#'@details To see the complete structure of the data.frame
#'@export
summary.ggedit=function(x, show.structure=FALSE, ...){
if(!inherits(x,'ggedit')) stop('Not ggedit object')

  out<-dput.ggedit(x)

  if(!show.structure) out<-gsub(',data=structure(.*?)class = "data.frame"\\)',',\\[data.frame\\]',out)
  
 writeLines(paste0('\n',names(out),'\n',out))
 
}


