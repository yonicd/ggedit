help.chunk=function(pkg,fn,output){

if(!is.character(fn)) fn=as.character(substitute(fn))

header=paste0("## ",fn)

chnk1=
 paste0(
 "```{r results='asis',echo=FALSE}
 fn.src('",fn,"','",pkg,"',hlp.dir)
 ```")

eval(parse(text=paste0('chnkExample<-readExample(',fn,')')))

if(output=='figure'){
  chnk2=paste0("```{r ",fn,", echo=FALSE,fig.show='hide',warning=FALSE}
")
  
  chnk3= paste0("
```
```{r}
picList=list.files(fig.dir,pattern = '",fn,"-',full.names = T)
slickR(picList,slideId = '",fn,"',width='100%',height='400px',slickOpts=list(dots=T))
```
")
  }
  
if(output=='table'){
chnk2=paste0(
"```{r ",fn,", echo=FALSE}
ex.out=example(",fn,",echo = F)$value
if(!is.list(ex.out)) ex.out=list(ex.out)
for(i in 1:length(ex.out)){
junk=texPreview(obj = ex.out[[i]],stem = paste0('",fn,"Ex',i),fileDir = fd,imgFormat = 'svg',ignore.stdout=T)
}
")


chnk3=paste0("
slickR(list.files(fd,pattern = glob2rx('",fn,"*.svg'),full.names = T),slideId = '",fn,"',width='100%',height='400px')
```
")
}
  



x=paste(header,chnk1,chnk2,chnkExample,chnk3,sep="
        \n
")

x

}

fn.src=function(fn,pkg,hlp.dir){
  pkgpaths <- find.package(pkg)
  fn.rd <- utils:::index.search(fn, pkgpaths, TRUE)
  fn.hlp=file.path(hlp.dir,paste0(fn,'Help.html'))
  
  tools::Rd2HTML(Rd = utils:::.getHelpFile(fn.rd),out = fn.hlp)
  #fn.hlp=paste0(capture.output({tools::Rd2HTML(Rd = utils:::.getHelpFile(fn.rd))}),collapse='\n')
  writeLines(paste0('<iframe width="100%" height="400" src="',fn.hlp,'" allowfullscreen></iframe>'))
}

createChapter=function(chapterName='Tables',chapterNum='01',funs=NULL){
  header<-paste0('# ',chapterName,'\n')
  if(!is.null(funs)) body<-sapply(funs$name,help.chunk,pkg=funs$pkg,output = funs$type)
  cat(c(header,body),sep = '\n',file = paste0(chapterNum,'-',chapterName,'.Rmd'))
}

queryChapter=function(pattern,field,pkg){
  x=lapply(pattern,function(y) help.search(y,fields = field,package = pkg,rebuild = F)$matches[,c('Package','Name','Entry')])
  out=do.call('rbind',x)
  if(field=='concept') out=out[out$Entry%in%c('figure','table'),]
  names(out)=c('pkg','Name','output')
  return(out)
}

readExample<-function (topic, package = NULL, lib.loc = NULL, character.only = FALSE, 
                       give.lines = FALSE, local = FALSE, echo = TRUE, verbose = getOption("verbose"), 
                       setRNG = FALSE, ask = getOption("example.ask"), prompt.prefix = abbreviate(topic, 
                                                                                                  6), run.dontrun = FALSE, run.donttest = interactive()) 
{

  if (!character.only) {
    topic <- substitute(topic)
    if (!is.character(topic)) 
      topic <- deparse(topic)[1L]
  }
  pkgpaths <- find.package(package, lib.loc, verbose = verbose)
  file <- utils:::index.search(topic, pkgpaths, TRUE)
  if (!length(file)) {
    warning(gettextf("no help found for %s", sQuote(topic)), 
            domain = NA)
    return(invisible())
  }
  packagePath <- dirname(dirname(file))
  pkgname <- basename(packagePath)
  lib <- dirname(packagePath)
  tf <- tempfile("Rex")
  tools::Rd2ex(utils:::.getHelpFile(file), tf, commentDontrun = !run.dontrun, 
               commentDonttest = !run.donttest)
  if (!file.exists(tf)) {
    if (give.lines) 
      return(character())
    warning(gettextf("%s has a help file but no examples", 
                     sQuote(topic)), domain = NA)
    return(invisible())
  }
  on.exit(unlink(tf))
  if (give.lines) 
    return(readLines(tf))
  if (pkgname != "base") 
    library(pkgname, lib.loc = lib, character.only = TRUE)
  if (!is.logical(setRNG) || setRNG) {
    if ((exists(".Random.seed", envir = .GlobalEnv))) {
      oldSeed <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", oldSeed, envir = .GlobalEnv), 
              add = TRUE)
    }
    else {
      oldRNG <- RNGkind()
      on.exit(RNGkind(oldRNG[1L], oldRNG[2L]), add = TRUE)
    }
    if (is.logical(setRNG)) {
      RNGkind("default", "default")
      set.seed(1)
    }
    else eval(setRNG)
  }
  x=readLines(tf)

  x=rmBlk('No test',x)
  x=rmBlk('Not run',x)
  paste(x,collapse='\n')
}

rmBlk=function(s,x){
  a=grep(s,x,fixed = T)
  if(length(a)==1) x=x[-a]
  if(length(a)==2) x=x[-c(a[1]:a[2])]
  if(length(a)>2) x=x[-unlist(sapply(data.frame(matrix(a,ncol=2,byrow = T)),function(y) seq(from=y[1],to=y[2])))]
  return(x)
}