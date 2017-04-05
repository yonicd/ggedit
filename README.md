[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/ggedit)](https://cran.r-project.org/package=ggedit)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active) 
![downloads](http://cranlogs.r-pkg.org/badges/grand-total/ggedit)

# ggedit

ggplot2 has become the standard of plotting in R for many users. New users, however, may find the learning curve steep at first, and more experienced users may find it challenging to keep track of all the options (especially in the theme!). 

ggedit is a package that helps users bridge the gap between making a plot and getting all of those pesky plot aesthetics just right, all while keeping everything portable for further research and collaboration.

ggedit is powered by a Shiny gadget where the user inputs a ggplot plot object or a list of ggplot objects. You can run ggedit directly from the console or from the Addin menu within RStudio.

Short clip from [rstudio::conf 2017](https://www.rstudio.com/conference/) (13:40-19:40)

<p><a href="https://www.rstudio.com/resources/videos/user-lightning-talks/?wvideo=64h36ke5ph"><img src="https://embedwistia-a.akamaihd.net/deliveries/14bd323d229d35c90ca8af815b0f49dde8f73ad2.jpg?image_play_button_size=2x&amp;image_crop_resized=960x540&amp;image_play_button=1&amp;image_play_button_color=71aadbe0" width="400" height="225" style="width: 400px; height: 225px;"></a></p><p><a href="https://www.rstudio.com/resources/videos/user-lightning-talks/?wvideo=64h36ke5ph">User Lightning Talks – RStudio</a></p>


## Installation

### CRAN
```r
install.packages('ggedit')
```

For a quick example, run the following:

```r
library('ggedit')
library(ggplot2)
p <- ggplot(mtcars, aes(x = hp, y = wt)) + geom_point() + geom_smooth()
p2 <- ggedit(p)
names(p2) # will show you which objects are available.
plot(p2) # shows the updated plot (it is available in the first element of p2)
```



### DEV
```r
devtools::install_github("metrumresearchgroup/ggedit")
```

#### DEV updates [ggedit_0.2.1](https://github.com/metrumresearchgroup/ggedit/blob/master/Miscellaneous/ggedit_0.2.1.tar.gz)

  - print.ggedit: S3 method to print directly from the output object to the console the script to reproduce  changes on the original plot object
  
```r
    print(obj)
    
    point
    geom_point(mapping=aes(colour=Species),alpha=0.5,size=3)+
    scale_colour_manual(values=c('#9E4A3F','#008B45','#6495ED'))
    
    pointWrap
    geom_point(mapping=aes(colour=Species),size=6)+
    theme(panel.background=element_rect(fill='white'))
    
    boxplotWrap
    geom_boxplot()
    
    pointLine
    geom_point(mapping=aes(shape=Species,colour=Petal.Width),size=6)+geom_line(linetype=2)
```

  - compare: compare two theme objects and return script or new theme object with only the differences between the two
  
```r
    compare(theme_bw(),theme_get())
    
    theme(legend.key=element_rect(fill='white'),
          panel.background=element_rect(fill='white'),
          panel.grid.major=element_line(colour='grey92'),
          panel.grid.minor=element_line(colour='grey92'),
          strip.background=element_rect(colour='grey20')
          )
```
  
  - call to ggedit is now ggedit(p.in,...), where the following arguments can be passed in to the  ellipses
    - viewer shiny viewer options. It can be either paneViewer (default with `minHeight=1000`), dialogViewer, browserViewer

    - verbose logical to control if the output includes script for layers and themes calls for parsing to create objects (default, `verbose=TRUE`)

    - showDefaults toggle to control if the verbose output shows all the input arguments passed to the proto object (if `verbose==FALSE` then ignored)

    - width,height dimensions of the renderPlot where the active plot is displayed
    

## Limitations
  - layers
    - non colour aesthetics of numeric inputs are not currently supported, e.g.:
      `iris%>%ggplot(aes(x=Sepal.Length,y=Sepal.Width))+geom_point()+geom_text(aes(label=Species,size=Sepal.Length))`
    - geom_text: family is not currently open to change
  - theme
    - margin,arrow are not currently available to edit