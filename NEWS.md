NEWS
================

-   [0.2.6](#section)
    -   [Update](#update)
    -   [Add](#add)
-   [0.2.2](#section-1)
    -   [Update](#update-1)
    -   [Add](#add-1)

0.2.6
=====

Update
------

Collapse internal files to run the module and gadget from same base script.

Reorganize BS Modals to have columns by object type.

Change plot.ggedit be a print.ggedit method.

`shinyBS` js library dependencies loaded with package, this lets ggedit run on attachment. i.e. `ggedit::ggedit(p)`

Add
---

`+.gg` functionality added to *gglist* class to manipulate to multiple plots in layout

``` r
as.gglist(list(pList[[1]],pList[[3]])) + 
  geom_hline(yintercept=3:4,linetype=2) + 
  theme_minimal()+
  theme(legend.position = 'top')
```

**gg\_session** retreives all functions that create ggroto layers or stats in current loaded namespace and returns unique mapping to their `position`,`geom`,`stat`.

``` r
gg_session()
```

**gg\_vetting** returns the columns from gg\_session relevant to a compiled plot

``` r
gg_vetting(pList$pointSmooth)
```

**ggedit\_opts** functionality to control session levels options (like `knitr::opts_chunk`). This can be used to manipulate the package defaults like the theme tips seen in BS modals. It is also used to store an updated output of gg\_session, so ggedit can identify the correct mapping with gg\_extension pacakges (eg `ggalt`).

``` r
library(ggalt)

ggedit_opts$set(list(session_geoms=gg_session()))

ggedit_opts$get('session_geoms')
```

manipulate gg\_extension pacakges (still in development, but works for a lot of the [ggplot2 extension packages](http://www.ggplot2-exts.org/))

**dput.ggedit** returns dput for ggplot2 object in script form and not a structure.

``` r
pList$pointSmooth #original compiled plot
this.gg <- dput.ggedit(pList$pointSmooth) #dput the plot
writeLines(this.gg) #show the output
eval(parse(text=this.gg)) #recompile the plot
```

**summary.ggedit** method for ggedit class return script that created compiled gg object.

``` r
out <- ggedit(pList[1:2])
#assuming out is returned from ggedit
summary(out)

# point
# ggplot(mapping=aes(x=Sepal.Length,y=Sepal.Width),[data.frame])+
# geom_point(aes(colour=Species),size=6)
# 
# pointWrap
# ggplot(mapping=aes(x=Sepal.Length,y=Sepal.Width),[data.frame])+
# geom_point(aes(colour=Species),size=6)+
# facet_wrap(facets=~Species,shrink=TRUE)
```

0.2.2
=====

Update
------

-   Change `mutate_each` with `mutate_all` to be compatible with new `dplyr` release

Add
---

    - can pass functions into stat_summary, eg `stat_summary(fun.y=mean_sd, geom='point')`
    - data.frames in layers are embedded as structures in verbose outputs, e.g. `geom_point(data=mtcars, aes(cyl, mpg))` will return
    ```r
    [1] "geom_point(mapping=aes(x=cyl,y=mpg), data=structure(list(mpg = c(21, 21, 22.8, 21.4, 18.7, 18.1, 14.3,
    24.4, 22.8, 19.2, 17.8, 16.4, 17.3, 15.2, 10.4, 10.4, 14.7, 32.4,.. <truncated>
    ```
    - aesthetic can be set to NULL, ie aes(group=NULL)
