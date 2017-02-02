library(ggedit)

p.in<-data.frame(x=0,y=0,xend=-1,yend=1,lbl="Waiting for Plot(s)",stringsAsFactors = F)%>%
  ggplot(aes(x=x,y=y,label=lbl))+
  geom_text(size=10)+scale_x_continuous(limits=c(-1,1))+scale_y_continuous(limits=c(-1,1))

if(is.ggplot(p.in)) p.in=list(p.in)

if(is.null(names(p.in))) names(p.in)=as.character(1:length(p.in))

if(!all(unlist(lapply(p.in,is.ggplot)))) stop("'object' is not a valid ggplot object")

if(!exists('minHeight')) minHeight=1000
uploadEnv=new.env()
uploadEnv$p.in
