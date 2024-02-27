[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/ggedit)](https://cran.r-project.org/package=ggedit)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/0.1.0/active.svg)](https://www.repostatus.org/#active) 
![downloads](https://cranlogs.r-pkg.org/badges/ggedit)[![Coverage Status](https://img.shields.io/codecov/c/github/yonicd/ggedit/master.svg)](https://app.codecov.io/github/yonicd/ggedit?branch=master)[![Covrpage Summary](https://img.shields.io/badge/covrpage-Last_Build_2020_06_01-yellowgreen.svg)](https://github.com/yonicd/ggedit/blob/master/tests/README.md)

# ggedit

ggplot2 has become the standard of plotting in R for many users. New users, however, may find the learning curve steep at first, and more experienced users may find it challenging to keep track of all the options (especially in the theme!). 

ggedit is a package that helps users bridge the gap between making a plot and getting all of those pesky plot aesthetics just right, all while keeping everything portable for further research and collaboration.

ggedit is powered by a Shiny gadget where the user inputs a ggplot plot object or a list of ggplot objects. You can run ggedit directly from the console or from the Addin menu within RStudio.

### Online User Manual Gitbook

A gitbook is maintained as the user manual for the package, you can access it here:

https://yonicd.github.io/ggedit/


Short clip from [rstudio::conf 2017](https://posit.co/resources/videos/lightning-talks-users/) (13:35-19:35)

<p><a href="https://posit.co/resources/videos/lightning-talks-users/?wvideo=64h36ke5ph"><img src="https://embed-fastly.wistia.com/deliveries/74f86de85f430b9ad819c3c5b04fc637.jpg?image_play_button_size=2x&amp;image_crop_resized=960x540&amp;image_play_button=1&amp;image_play_button_color=71aadbe0" style="width: 400px; height: 225px;" width="400" height="225"></a></p><p><a href="https://posit.co/resources/videos/lightning-talks-users/?wvideo=64h36ke5ph">Lightning Talks - Users - RStudio</a></p>


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
remotes::install_github("yonicd/ggedit")
```

## Limitations
  - layers
    - non colour aesthetics of numeric inputs are not currently supported, e.g.:

      ```
      iris |> 
        ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
        geom_point() + 
        geom_text(aes(label = Species, size = Sepal.Length))
      ```
    - `geom_text`: family is not currently open to change
