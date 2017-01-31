#' @export
themeTips=list(
     element_rect=list(
                        fill='fill colour',
                        colour='border colour',
                        size='border size (in pts)',
                        linetype=paste0(paste(seq(0,6),
                           c("blank","solid", "dashed", "dotted", "dotdash", 
                             "longdash", "twodash"),sep=": "),
                           collapse=", ")),
     element_line=list(
              colour='line colour',
               size='numeric (in pts) or \n relative to global size rel(numeric)',
               linetype=paste0(paste(seq(0,6),
                                     c("blank","solid", "dashed", "dotted", "dotdash", 
                                       "longdash", "twodash"),sep=": "),
                               collapse=", "),
               lineend=c("butt(default),round,square")),
     element_text=list(family=HTML('<a href="http://www.cookbook-r.com/Graphs/Fonts/" target="_blank">font family</a>'),
                       face='font face ("plain", "italic", "bold", "bold.italic")',
                       colour='text colour',
                       size='text size (in pts)',
                       hjust='horizontal justification (in [0, 1])',
                       vjust='vertical justification (in [0, 1])',
                       angle='angle (in [0, 360])',
                       lineheight='numeric line height'),
     justification=list(justification='anchor point for positioning legend inside plot <br/> "center" or two-element numeric vector'),
     position=list(position='the position of legends. <br/> "left", "right", "bottom", "top", or two-element numeric vector')
     )

save(themeTips,file='ggedit/data/themeTips.rda')
