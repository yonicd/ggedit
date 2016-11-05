# ggedit
Interactively edit ggplot layer aesthetics and theme definitions

Depends: R (>= 2.3.0), shiny, shinyBS, reshape2, grid, gridExtra, scales, ggplot2 (>= 2.1.0), plyr, colourpicker (>= 0.2), dplyr (>= 0.5), miniUI

ggedit(p.in,viewer)

**Arguments**

**p.in** ggplot object or list of ggplot objects

**viewer** paneViewer(),dialogViewer() or browserViewer()

**Details** 

Can be called from the console or from the addin menu. if called from the addin menu highlight in the editor pane the object the will pass through to the function.

**Value**

returns a list of objects

**UpdatedPlots** list of ggplot object with updated aesthetics and themes

**UpdatedLayers** list of all the layer objects from the UpdatedPlots

**UpdatedTheme** list of ggplot theme objects of the UpdatedPlots