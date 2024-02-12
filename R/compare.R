#' @title compare
#' @description Compare differences theme object e1 (new theme) to theme object e2 (old theme)
#' @export
#' @param e1 theme object
#' @param e2 theme object
#' @param verbose logical to control if the output is a character of script or a theme object (default TRUE)
#' @return theme object or character depending on verbose
#' @examples
#' compare(ggplot2::theme_bw(),ggplot2::theme_get())
#' compare(ggplot2::theme_bw(),ggplot2::theme_get(),verbose=FALSE)
#' @importFrom tidyr spread
#' @importFrom tibble as_tibble
#' @importFrom purrr map_df
#' @importFrom rlang sym syms '!!!' '!!'
compare <- function(e1, e2, verbose=TRUE) {
  objL <- lapply(list(compare = e1, base = e2), function(x) {
    objList <- themeFetchFull(x)

    objListDepth <- sapply(objList, themeListDepth)

    return(list(x = x, objList = objList, objListDepth = objListDepth))
  })

  objDF <- purrr::map_df(objL, .f = function(x) {
    
    dfOut <- dplyr::bind_rows(
      x$objList[x$objListDepth == 1]  |> 
        purrr::map_df(.f = function(x) {

            purrr::map_df(x[-length(x)],
                        function(x) tibble::as_tibble(t(x)),
                        .id = 'element') |> 
            dplyr::mutate(call = x$call)
        
        }, .id = "Theme")  |> 
        dplyr::mutate(subTheme = NA)  |> 
        dplyr::mutate_all(as.character),

      x$objList[!x$objListDepth == 1]  |> 
        purrr::map_df(
          .f = function(y) y  |> 
            purrr::map_df(.f = function(x) {
              
              purrr::map_df(x[-length(x)],
                            function(x) tibble::as_tibble(t(x)),
                            .id = 'element') |> 
                dplyr::mutate(call = x$call)
              
              }, .id = "subTheme"),
          .id = "Theme"
        )  |> 
        dplyr::mutate_all(as.character)
    )

    dfOut$subTheme <- ifelse(is.na(dfOut$subTheme), "", dfOut$subTheme)
    dfOut$Theme <- factor(dfOut$Theme, levels = names(x$objList))
    dfOut$value <- ifelse(dfOut$value %in% c("", NA), ".", dfOut$value)
    dfOut$elem_num <- 1:nrow(dfOut)

    dfOut$class <- ifelse(dfOut$call == "unit", "unit", dfOut$class)
    dfOut$value <- ifelse(grepl("#", dfOut$value), gsub("#", "'#'*", dfOut$value), dfOut$value)
    #dfOut$value <- ifelse(grepl("^[.]$", dfOut$value), NA, dfOut$value)
    dfOut$value <- ifelse(grepl("TRUE|FALSE|[=]", dfOut$value), paste0("'", dfOut$value, "'"), dfOut$value)
    dfOut$call <- ifelse(dfOut$call %in% c("character", "unit", dfOut$subTheme), "", dfOut$call)
    dfOut$element <- ifelse(dfOut$element == dfOut$subTheme, "", dfOut$element)

    return(dfOut)
  }, .id = "idTheme")

  d <- objDF  |> 
    dplyr::select(!!rlang::sym('idTheme'):!!rlang::sym('value'), !!rlang::sym('subTheme')) |> 
    tidyr::spread(!!rlang::sym('idTheme'),!!rlang::sym('value'))  |>  #,fill='.'
    dplyr::filter(!!rlang::sym('compare') != !!rlang::sym('base'))  |> 
    dplyr::left_join(objDF, by = c("Theme", "subTheme", "element", "name"))  |> 
    dplyr::filter(!!rlang::sym("idTheme") == 'compare')  |> 
    dplyr::select(!!!rlang::syms(c("Theme", "subTheme", "call", "element")), value = !!rlang::sym("compare"))

  d$Theme <- as.character(d$Theme)
  
  d <- d[d$value != ".", ]
  d$value[grepl("[^0-9]", d$value)] <- paste0("'", d$value[grepl("[^0-9]", d$value)], "'")
  
  #d$value[d$value== "."] <- NA
  #d$value[grepl("[^0-9]", d$value)&!grepl('^(TRUE|FALSE)$',d$value)] <- paste0("'", d$value[grepl("[^0-9]", d$value)&!grepl('^(TRUE|FALSE)$',d$value)], "'")

  x1 <- d  |> 
    dplyr::group_by(Theme, subTheme, call)  |>
    dplyr::summarise(y = paste0(paste0(element, '=', value),collapse = ','))  |>
    dplyr::ungroup()

  x1$y <- ifelse(x1$call == "", gsub("=", "", x1$y), paste0(x1$call, "(", x1$y, ")"))
  
  # x1$y <- gsub("''TRUE''","TRUE",x1$y)
  # x1$y <- gsub("''FALSE''","FALSE",x1$y)
  
  x1$x <- ifelse(x1$subTheme == "", x1$Theme, paste(x1$Theme, x1$subTheme, sep = "."))
  out <- paste0("theme(", paste0(paste(x1$x, x1$y, sep = "="), collapse = ","), ")")

  if (verbose) {
    out
  } else {
    eval(parse(text = out))
  }
}

#' @importFrom utils globalVariables
utils::globalVariables(c('Theme', 'subTheme', 'element', 'value'))
