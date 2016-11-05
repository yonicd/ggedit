pList=list(
  iris%>%ggplot(aes(x =Sepal.Length,y=Sepal.Width))+
    geom_point(aes(colour=Species),size=6),
  
  iris%>%ggplot(aes(x =Sepal.Length,y=Sepal.Width))+
    geom_point(aes(colour=Species),size=6)+
    facet_wrap(~Species,scales='free')+labs(title='a'),
  
  iris%>%mutate(Petal2=factor(Petal.Length>2,labels=c("Long Petal",'Short Petal')))%>%
    ggplot(aes(x =Species,y=Sepal.Width,fill=Species))+
    geom_boxplot()+facet_wrap(~Petal2,scales='free_x'),
  
  iris%>%ggplot(aes(x =Sepal.Length,y=Sepal.Width))+
    geom_point(aes(shape=Species,colour=Petal.Width),size=6)+
    geom_line(linetype=2),
  iris%>%ggplot(aes(x =Sepal.Length,y=Sepal.Width))+
    geom_point(aes(shape=Species,colour=Petal.Width),size=6)+
    geom_smooth(method='loess'),
  
  iris%>%ggplot(aes(x =Sepal.Length,y=Sepal.Width))+
    geom_point(aes(colour=Species),size=6)+
    geom_line(linetype=2)+
    facet_wrap(~Species,scales='free'),
  
  iris%>%do(.,cbind(.,r=1:nrow(.),id=sample(c('a','b','c','d'),nrow(.),replace = T)))%>%
    ggplot(aes(x=Sepal.Length,y=Sepal.Width,group=Species))+
    geom_point(aes(colour=id))+geom_line()+
    facet_grid(Species~id)+
    list()
)