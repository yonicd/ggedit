themePanel=function(obj.in,obj.name){
  lapply(names(obj.in) ,FUN = function(item){
    do.call(what = tabPanel,
            args = list(title=item,
                        lapply(head(names(obj.in[[item]]),-1),
                               FUN=function(subitem){
                                 x=obj.in[[item]][[subitem]]
                                 txt.in=unique(c(obj.name,item,subitem))
                                 if(obj.name=="legend"&item%in%c("position","justification")) txt.in=c(obj.name,item,subitem)
                                 obj=textInput(inputId = paste0(c("pop",txt.in),collapse=""),
                                               label = x['name'],
                                               value = x['value']
                                 )
                                 return(obj)
                               })
            )
    )
  }
  )
}