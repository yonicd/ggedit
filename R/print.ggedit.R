#'@title Print verbose outputs of objects of class ggedit
#'@description function to tidy the ggedit output to single script calls for each plot
#'@param x ggedit object
#'@param ... nothing
#'@export
print.ggedit=function(x,...){
if(!inherits(x,'ggedit')) stop('Not ggedit object')

  calls=sapply(grep('Calls',names(x),value = T),
              function(y){
                sapply(x[[y]],function(obj) {
                  paste0(obj,collapse='+') 
                })
              })

 out=apply(calls,1,function(obj){
   paste(obj[!obj%in%c('list()','')],collapse='+\n') 
 })

 writeLines(paste0('\n',names(out),'\n',out))
 
}


