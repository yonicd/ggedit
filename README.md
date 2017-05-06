[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/ggedit)](https://cran.r-project.org/package=ggedit)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active) 
![downloads](http://cranlogs.r-pkg.org/badges/grand-total/ggedit)

# ggedit

ggplot2 has become the standard of plotting in R for many users. New users, however, may find the learning curve steep at first, and more experienced users may find it challenging to keep track of all the options (especially in the theme!). 

ggedit is a package that helps users bridge the gap between making a plot and getting all of those pesky plot aesthetics just right, all while keeping everything portable for further research and collaboration.

ggedit is powered by a Shiny gadget where the user inputs a ggplot plot object or a list of ggplot objects. You can run ggedit directly from the console or from the Addin menu within RStudio.

### Online User Manual Gitbook

A gitbook is maintained as the user manual for the package, you can access it here:

https://metrumresearchgroup.github.io/ggedit/


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

#### DEV updates [ggedit_0.2.2](https://github.com/metrumresearchgroup/ggedit/blob/master/Miscellaneous/ggedit_0.2.2.tar.gz)

  - replaced `mutate_each` with `mutate_all` to be compatible with new `dplyr` release

  - functionality added
    - can pass functions into stat_summary, eg `stat_summary(fun.y=mean_sd, geom='point')`
    - data.frames in layers are embedded as structures in verbose outputs, e.g. `geom_point(data=mtcars, aes(cyl, mpg))` will return
    ```r
    [1] "geom_point(mapping=aes(x=cyl,y=mpg), data=structure(list(mpg = c(21, 21, 22.8, 21.4, 18.7, 18.1, 14.3, \n24.4, 22.8, 19.2, 17.8, 16.4, 17.3, 15.2, 10.4, 10.4, 14.7, 32.4,.. <truncated>
    ```
    - aesthetic can be set to NULL, ie aes(group=NULL)
    
example plot

```r
plot <- 
  iris%>%
  ggplot(aes(x=cut(Sepal.Width,2),y=Sepal.Width)) +
  aes(colour=Species,group=Species)+
  geom_point(alpha = 0.5,shape = 16, size = 1) +
  geom_boxplot(aes(group=NULL), varwidth = FALSE,notch = FALSE, show.legend = TRUE)+
  stat_summary(fun.y = 'median', geom = "line")+
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom",
        legend.box = "vertical", legend.direction = "horizontal",
        axis.text.x = ggplot2::element_text(angle = 90,hjust = 1, vjust = 0.5),
        legend.title = element_blank())

ggedit(plot)
```
    

## Limitations
  - layers
    - non colour aesthetics of numeric inputs are not currently supported, e.g.:
      `iris%>%ggplot(aes(x=Sepal.Length,y=Sepal.Width))+geom_point()+geom_text(aes(label=Species,size=Sepal.Length))`
    - geom_text: family is not currently open to change
  - theme
    - margin,arrow are not currently available to edit