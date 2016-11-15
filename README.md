# ggedit

ggplot2 has become the standard of plotting in R for many users. To create a simple or difficult plot is something nearly everyone can do. But for new users there is a learning curve that may seem steep at first and for more experienced users keeping track of all the options (especially in the theme!) can become cumbersome. 

ggedit is a package that helps users fill the gap between making the plot and getting those pesky plot aesthetics just right, while keeping everything portable for further research and collaboration.

ggedit is powered by a shiny gadget where the user inputs a ggplot plot object or a list of ggplot objects. It can be run either from the console directly or if you are using RStudio you can also run it from the Addin menu.

##Installation
```r
devtools::install_github("metrumresearchgroup/ggedit",subdir="ggedit")
```

##Layers
The gadget creates a popup window which is populated by the information found in each layer. Then the user can change any aesthetic value found in a layer and see the changes happen in real time.


[![Using ggedit on plot layers](http://img.youtube.com/vi/OvMWCHpCmaI/0.jpg)](http://www.youtube.com/watch?v=OvMWCHpCmaI)


To prevent any changes to the original plot the changed layers are cloned from the original plot object and are independent of it. They are also given in the output as objects so you can use them regardless of plot using regular ggplot2 grammar. This is a great advantage when collaborating with other people. You can send over a plot the other people can edit the layers aesthetics and send back only the new layers where you can implement them and continue to work. 

##Themes
ggedit also has a theme editor inside. The user can edit any element in the theme and see the changes happen in real time, making the trial and error process quick and easy. Once you like the theme you made and you have a list of plots you can transfer the theme to the other plots with one click or even make the theme the session theme regardless of the gadget. As with the layers the new theme object is also part of the output so you can collaborate easily.

[![Using ggedit on plot themes](http://img.youtube.com/vi/813QxbsrvLM/0.jpg)](http://www.youtube.com/watch?v=813QxbsrvLM)


##Outputs
The gadget returns a list containing 4 elements

  - updatedPlots
    - list containing updated ggplot objects
  - updatedLayers
    - For each plot a list of updated layers (ggproto) objects
    - portable object
  - UpdatedLayersElements
    - For each plot a list elements and their values in each layer
    - Can be used to update the new values in the original code
  - updatedThemes
    - For each plot a list of updated theme objects
    - portable object
    - If the user doesn't edit the theme updatedThemes will not be returned

##rgg
After you finish editing the plots the natural progression is to use them in the rest of the script. In ggedit there is the function rgg (remove and replace ggplot). Using this function you can chain into the original code changes to the plot without multiplying script needlessly.


[![Using ggedit with ggplot2 objects](http://img.youtube.com/vi/0QmJHwoWQWM/0.jpg)](http://www.youtube.com/watch?v=0QmJHwoWQWM)


With this function you can 

specify which layer you want to remove from a plot:

`ggObj%>%rgg('line')`

if there are more than one of the same type of layer just specify the index of the one you want to remove

`ggObj%>%rgg('line',2)`

remove a layer from ggObj and replace it with a new one from the ggedit output p.out

`ggObj%>%rgg('line',newLayer = p.out$UpdatedLayers)`

remove a layer, replace it with a new one and the new theme 

`ggObj%>%rgg('line',newLayer = p.out$UpdatedLayers)+p.out$UpdatedThemes`

There is also a plotting function for ggedit objects that creates a grid.view for you and finds the best grid size for the amount of plots you have in the list. And for the exotic layouts you can give specific positions and the rest will be done for you. If you didn't use ggedit you can still add the class to any ggplot and use the ploting function just the same.

```r
plot(as.ggedit(list(p0,p1,p2,p3)),list(list(rows=1,cols=1:3),
                                       list(rows=2,cols=2),
                                       list(rows=2,cols=1),
                                       list(rows=2,cols=3))
)
```

##Addin Launch
To launch the gadget on from the addin menu. Highlight the code that creates the plot object or the plot name in the source pane of Rstudio and the click on the Addins from the dropdown menu.

[![Launching ggedit from the Addins menu](http://img.youtube.com/vi/693XhHt8fug/0.jpg)](http://www.youtube.com/watch?v=693XhHt8fug)
