#Metrum Research Group (@MetrumRG)
##Jonathan Sidi (@yoniceedee)

rm(list=ls())
library(ggedit)
#?ggedit

p0=list(
  Scatter=iris%>%ggplot(aes(x =Sepal.Length,y=Sepal.Width))+
    geom_point(aes(colour=Species),size=6),
  
  ScatterFacet=iris%>%ggplot(aes(x =Sepal.Length,y=Sepal.Width))+
    geom_point(aes(colour=Species),size=6)+
      geom_line(linetype=2)+
    facet_wrap(~Species,scales='free')+
    labs(title='Some Title')
  )

a=ggedit(p.in = p0,verbose = T)

#load('RstudioExampleObj.rda') #pre-run example

ldply(a,names)

plot(a)

comparePlots=c(p0,a$UpdatedPlots)
names(comparePlots)[c(3:4)]=paste0(names(comparePlots)[c(3:4)],"Updated")

#Initial Comparison Plot
plot(as.ggedit(comparePlots))

#Apply updated theme of first plot to second plot
comparePlots$ScatterFacetNewTheme=p0$ScatterFacet+a$UpdatedThemes$Scatter

plot(as.ggedit(comparePlots[c("ScatterFacet","ScatterFacetNewTheme")]),
      plot.layout = list(list(rows=1,cols=1),list(rows=2,cols=1))
     )


#Using Remove and Replace Function

#Overlay two layers of same geom
(comparePlots$ScatterMistake=p0$Scatter+a$UpdatedLayers$ScatterFacet[[1]])

#Remove
(comparePlots$ScatterNoLayer=p0$Scatter%>%
  rgg(oldGeom = 'point'))

#Replace Geom_Point layer on Scatter Plot
(comparePlots$ScatterNewLayer=p0$Scatter%>%
  rgg(oldGeom = 'point',
      oldGeomIdx = 1,
      newLayer = a$UpdatedLayers$ScatterFacet[[1]]))

#Remove and Replace Geom_Point layer and add the new theme
(comparePlots$ScatterNewLayerTheme=p0$Scatter%>%
  rgg(oldGeom = 'point',
      newLayer = a$UpdatedLayers$ScatterFacet[[1]])+
  a$UpdatedThemes$Scatter)

#plot(as.ggedit(comparePlots))

#Cloning Layers
##A geom_point layer
  (l=p0$Scatter$layers[[1]])

##Clone the layer
  (l1=cloneLayer(l))

###Test that all is equal
  all.equal(l,l1)

###Verbose copy of layer
  (l1.txt=cloneLayer(l,verbose = T))
  
###Parse the text
  (l2=eval(parse(text=l1.txt)))
  
###Test that all is equal
  all.equal(l,l2)

#Back to our example
  #Original geom_point layer
  parse(text=cloneLayer(p0$ScatterFacet$layers[[1]],verbose = T))
  #new Layer
  parse(text=a$UpdatedLayerCalls$ScatterFacet[[1]])
  
#Visualize Themes
pTheme=list()
(pTheme$Base=plot(a$UpdatedThemes$Scatter))
(pTheme$Select=plot(a$UpdatedThemes$Scatter,themePart = c('plot','legend'),fnt = 18))

#Visually Compare Theme
(pTheme$Compare=plot(obj=a$UpdatedThemes$Scatter,obj2 = ggplot2:::theme_get()))

