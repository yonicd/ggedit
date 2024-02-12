
pList=list(
  #1
  point = iris |> 
    ggplot(aes(x = Sepal.Length, y=Sepal.Width))+
    geom_point(aes(colour=Species), size=6),
  #2
  pointWrap = iris |> 
    ggplot(aes(x =Sepal.Length,y=Sepal.Width))+
    geom_point(aes(colour=Species),size=6)+
    facet_wrap(vars(Species),scales='free')+labs(title='a'),
  #3
  boxplotWrap=iris |> 
    dplyr::mutate(Petal2=factor(Petal.Length>2,labels=c("Long Petal",'Short Petal'))) |> 
    ggplot(aes(x =Species,y=Sepal.Width,fill=Species))+
    geom_boxplot()+facet_wrap(vars(Petal2),scales='free_x'),
  #4
  pointLine=iris |> 
    ggplot(aes(x =Sepal.Length,y=Sepal.Width))+
    geom_point(aes(shape=Species,colour=Petal.Width),size=6)+
    geom_line(linetype=2),
  #5
  pointSmooth=iris |> 
    ggplot(aes(x =Sepal.Length,y=Sepal.Width))+
    geom_point(aes(shape=Species,colour=Petal.Width),size=6)+
    geom_smooth(method='loess'),
  #6
  pointLineWrap=iris |> 
    ggplot(aes(x =Sepal.Length,y=Sepal.Width))+
    geom_point(aes(colour=Species),size=6)+
    geom_line(linetype=2)+
    facet_wrap(vars(Species),scales='free'),
  #7
  pointLineGrid = iris |> 
    dplyr::mutate(
      id = sample(letters[1:5], dplyr::n(), replace = TRUE)
    ) |> 
    ggplot(aes(x=Sepal.Length,y=Sepal.Width,group=Species))+
    geom_point(aes(colour=id))+geom_line()+
    facet_grid(rows = vars(Species), cols = vars(id))
)

usethis::use_data(pList, overwrite = TRUE)
