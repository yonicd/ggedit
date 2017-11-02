#' @name ggEdit_Module
#' @title ggEdit
#' @description The ggedit module.
#' @param input shinyapp input argument
#' @param output shinyapp output argument
#' @param session shinyapp session argument
#' @param obj ggplot as reactive shiny object
#' @param verbose logical to control if the output includes script for layers and themes calls for parsing to create objects (default, verbose=F)
#' @param showDefaults toggle to control if the verbose output shows all the input arguments passed to the proto object (if verbose==FALSE then ignored)
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @export
#' @keywords internal
#' @importFrom scales brewer_pal linetype_pal
#' @importFrom shinyAce aceEditor
#' @importFrom shinyBS bsModal
#' @importFrom utils capture.output
ggEdit <- function(input, output, session, obj, verbose=TRUE, showDefaults=FALSE, width="auto", height="auto") {
  TEMPLIST <- new.env()

  shiny::observe({
    TEMPLIST$objList.new <- list()

    p.in <- obj()

    if (is.ggplot(p.in)) {
      p.in <- list(p.in)
    }

    if (is.null(names(p.in))) {
      names(p.in) <- as.character(1:length(p.in))
    }

    if (!all(unlist(lapply(p.in, is.ggplot)))) {
      stop("'object' is not a valid ggplot object")
    }

    TEMPLIST$objList.new <- p.in
    TEMPLIST$obj.theme <- vector("list", length(TEMPLIST$objList.new))
    TEMPLIST$nonLayers <- vector("list", length(TEMPLIST$objList.new))
    TEMPLIST$nonLayersTxt <- vector("list", length(TEMPLIST$objList.new))
  })


  # populate select with the plots chosen to work on
  output$activePlot <- shiny::renderUI({
    ns <- session$ns

    nm <- factor(names(TEMPLIST$objList.new), ordered = TRUE, levels = names(TEMPLIST$objList.new))

    shiny::selectInput(ns("activePlot"), "Choose Plot:", choices = split(1:length(nm), nm), selected = 1)
  })

  baseLayerVerbose <- shiny::eventReactive(input$activePlot, {
    p.in <- obj()
    if (is.ggplot(p.in)) {
      p.in <- list(p.in)
    }

    if (is.null(names(p.in))) {
      names(p.in) <- as.character(1:length(p.in))
    }

    lapply(p.in, function(x) lapply(x$layers, function(y) cloneLayer(y, verbose = TRUE, showDefaults = showDefaults)))
  })

  plotIdx <- shiny::eventReactive(input$activePlot, {
    if (is.null(input$activePlot)) {
      1
    } else {
      as.numeric(input$activePlot)
    }
  })

  shiny::observe(TEMPLIST$obj.new <- TEMPLIST$objList.new[[plotIdx()]])

  theme.now <- ggplot2::theme_get()

  # Layers----

  shiny::observeEvent(input$activePlot, {
    output$layers <- shiny::renderUI({
      ns <- session$ns

      shiny::radioButtons(ns("geoms"), "Choose layer(s):", choices = geom_list(TEMPLIST$obj.new), selected = geom_list(TEMPLIST$obj.new)[1], inline = TRUE)
    })

    TEMPLIST$obj.theme <- lapply(TEMPLIST$objList.new, function(p) {
      if (length(p$theme) > 0) {
        theme.now <- theme.now %+replace% p$theme
      }

      themeFetch(theme.now)
    })
  })

  update.Layer <- shiny::eventReactive(input$sendElem, {
    TEMPLIST$obj.new <- TEMPLIST$objList.new[[as.numeric(input$activePlot)]]

    layer.idx <- which(geom_list(TEMPLIST$obj.new) == input$geoms)

    numElem <- unlist(lapply(TEMPLIST$obj.Elems[[layer.idx]], function(x) length(x$val[[1]])))

    for (item in names(TEMPLIST$obj.Elems[[layer.idx]])) {
      if (numElem[item] == 1) {
        newLayer <- cloneLayer(TEMPLIST$obj.new$layers[[layer.idx]])
        newLayer$aes_params[[item]] <- eval(parse(text = paste0("input$pop", toupper(item))))
        TEMPLIST$obj.new$layers[[layer.idx]] <- newLayer
      } else {
        if (TEMPLIST$obj.Elems[[layer.idx]][[item]][["class"]][[1]] == "numeric") {
          if (input[[paste0("pop", toupper(item), "fixedPal")]] != "Manual") {
            palItem <- paste0("'", input[[paste0("pop", toupper(item), "fixedPal")]], "'")

            palTxt <- paste0("scale_", item, "_gradientn(colours=scales::brewer_pal(palette=", palItem, ",direction=-1)(9)[1:5])")

            TEMPLIST$nonLayersTxt[[as.numeric(input$activePlot)]][[paste0("scale_", item, "_gradientn")]] <- palTxt

            suppressMessages({
              nL <- eval(parse(text = palTxt))
            })

            TEMPLIST$nonLayers[[as.numeric(input$activePlot)]][[paste0("scale_", item, "_gradientn")]] <- nL

            suppressMessages({
              eval(parse(text = paste0("TEMPLIST$obj.new <- TEMPLIST$obj.new + ", palTxt)))
            })
          } else {
            LowCol <- paste0("'", input[[paste0("pop", input$pop, toupper(item), "Low")]], "'")

            HighCol <- paste0("'", input[[paste0("pop", input$pop, toupper(item), "High")]], "'")

            ColTxt <- paste0("scale_", item, "_gradient(low=", LowCol, ",high=", HighCol, ")")

            TEMPLIST$nonLayersTxt[[as.numeric(input$activePlot)]][[paste0("scale_", item, "_gradient")]] <- ColTxt

            suppressMessages({
              nL <- eval(parse(text = ColTxt))
            })

            TEMPLIST$nonLayers[[as.numeric(input$activePlot)]][[paste0("scale_", item, "_gradient")]] <- nL

            suppressMessages({
              eval(parse(text = paste0("TEMPLIST$obj.new <- TEMPLIST$obj.new + ", ColTxt)))
            })
          }
        } else {
          vals <- unlist(lapply(names(input)[grepl(paste0("pop", toupper(item), "[1-9]"), names(input))], function(x) input[[x]]))

          if (!item %in% c("size", "shape", "linetype")) {
            vals <- paste0("'", vals, "'")
          }

          if (item == "linetype") {
            vals <- match(vals, c("0", scales::linetype_pal()(6))) - 1
          }

          vals <- paste0(vals, collapse = ",")

          TEMPLIST$nonLayersTxt[[as.numeric(input$activePlot)]][[paste0("scale_", item, "_manual")]] <- paste0("scale_", item, "_manual(values=c(", vals, "))")

          suppressMessages({
            nL <- eval(parse(text = paste0("scale_", item, "_manual(values=c(", vals, "))")))
          })

          TEMPLIST$nonLayers[[as.numeric(input$activePlot)]][[paste0("scale_", item, "_manual")]] <- nL

          suppressMessages(eval(parse(text = paste0("TEMPLIST$obj.new <- TEMPLIST$obj.new + scale_", item, "_manual(values=c(", vals, "))"))))
        }
      }
    }

    TEMPLIST$objList.new[[as.numeric(input$activePlot)]] <- TEMPLIST$obj.new

    return(TEMPLIST$objList.new)
  })

  output$popElems <- shiny::renderUI({
    ns <- session$ns

    if (is.null(input$activePlot)) {
      aP <- 1
    } else {
      aP <- as.numeric(input$activePlot)
    }

    TEMPLIST$obj.new <- TEMPLIST$objList.new[[aP]]

    TEMPLIST$obj.Elems <- fetch_aes_ggplotBuild(TEMPLIST$obj.new, geom_list(TEMPLIST$obj.new))

    if (is.null(input$geoms)) {
      gIdx <- 1
    } else {
      gIdx <- input$geoms
    }

    obj.elems <- TEMPLIST$obj.Elems[[gIdx]]

    obj.elems <- obj.elems[!names(obj.elems) %in% c("family")]

    obj.elemsL <- list()

    for (item in names(obj.elems)) {
      item_class <- obj.elems[[item]]$class[[1]]

      if (item %in% c("colour", "color", "fill")) {
        divName <- "divColor"

        if (is.null(obj.elemsL[[divName]])) {
          obj.elemsL[[divName]] <- list()
        }
      } else {
        if (item_class == "data.frame") {
          divName <- "divSlide"

          if (is.null(obj.elemsL[[divName]])) {
            obj.elemsL[[divName]] <- list()
          }
        }

        if (item_class %in% c("character", "factor")) {
          divName <- "divSelect"

          if (is.null(obj.elemsL[[divName]])) {
            obj.elemsL[[divName]] <- list()
          }
        }
      }

      obj.elemsL[[divName]][[item]] <- obj.elems[[item]]
    }


    shinyBS::bsModal(
      id = ns("updateElemPopup"), title = "Update Plot Layer", trigger = ns("updateElem"), size = "large",

      shiny::fluidRow(
        lapply(obj.elemsL, function(objItem) {
          shiny::column(
            4,
            lapply(names(objItem), FUN = function(item) {
              list(
                lapply(arg.value(item, objItem, session), function(x) {
                  do.call(what = x[["type"]], args = x[["args"]])
                })
              )
            })
          )
        })
      ),
      shiny::div(align = "right", shiny::actionButton(ns("sendElem"), "Update Layer"))
    )
  })

  # Theme----

  update.Theme <- shiny::eventReactive(input$sendTheme, {
    TEMPLIST$obj.new <- TEMPLIST$objList.new[[as.numeric(input$activePlot)]]

    strThemeCallList <- lapply(names(TEMPLIST$obj.theme[[plotIdx()]]), function(item) {
      themeNewVal(TEMPLIST$obj.theme[[plotIdx()]][item], TEMPLIST$obj.new, input)
    })

    strThemeCall <- paste0("TEMPLIST$obj.new <- TEMPLIST$obj.new + theme(", paste0(unlist(strThemeCallList), collapse = ","), ")")

    eval(parse(text = strThemeCall))

    TEMPLIST$objList.new[[as.numeric(input$activePlot)]] <- TEMPLIST$obj.new

    TEMPLIST$themeUpdate <- lapply(TEMPLIST$objList.new, function(p) p$theme)

    return(TEMPLIST$objList.new)
  })

  shiny::observeEvent(input$SetThemeGlobal, {
    if (length(TEMPLIST$obj.new$theme) > 0) {
      theme.now <- theme.now + TEMPLIST$obj.new$theme
    }

    ggplot2::theme_set(theme_get() %+replace% theme.now)
  })

  update.ThemeGrid <- shiny::eventReactive(input$SetThemeGrid, {
    p.now <- TEMPLIST$objList.new[[as.numeric(input$activePlot)]]

    if (length(p.now$theme) > 0) {
      theme.now <- theme.now + p.now$theme
    }

    for (i in 1:length(TEMPLIST$objList.new)) {
      TEMPLIST$objList.new[[i]] <- TEMPLIST$objList.new[[i]] + theme.now

      TEMPLIST$themeUpdate[[i]] <- TEMPLIST$objList.new[[i]]$theme
    }

    return(TEMPLIST$objList.new)
  })

  shiny::observeEvent(input$activePlot, {
    output$popTheme <- shiny::renderUI({
      ns <- session$ns

      shinyBS::bsModal(
        id = ns("updateThemePopup"), title = shiny::HTML('Update Plot Theme <a href="http://docs.ggplot2.org/0.9.3.1/theme.html" target="_blank">(help)</a>'), trigger = ns("updateTheme"), size = "large",

        do.call(
          shiny::tabsetPanel,
          unlist(lapply(1:length(TEMPLIST$obj.theme[[plotIdx()]]), FUN = function(j) {
            if (themeListDepth(TEMPLIST$obj.theme[[plotIdx()]][j]) > 2) {
              list(themeMakePanel(TEMPLIST$obj.theme[[plotIdx()]][j], session = session))
            } else {
              unlist(lapply(j, function(i) {
                themeMakePanel(TEMPLIST$obj.theme[[plotIdx()]][i], session = session)
              }), recursive = FALSE)
            }
          }), recursive = FALSE)
        ),

        shiny::hr(),

        shiny::div(align = "right", shiny::actionButton(ns("sendTheme"), "Set Theme"))
      )
    })
  })

  # Render Plot----

  output$Plot <- shiny::renderPlot({
    as.ggedit(TEMPLIST$objList.new)
  }, width = width, height = height)

  shiny::observeEvent(input$updateElem, {
    output$Plot <- shiny::renderPlot({
      if (input$sendElem == 0) {
        as.ggedit(TEMPLIST$objList.new)
      } else {
        pList.out <- update.Layer()

        as.ggedit(pList.out)
      }
    }, width = width, height = height)
  })

  shiny::observeEvent(input$updateTheme, {
    output$Plot <- shiny::renderPlot({
      if (input$sendTheme == 0) {
        as.ggedit(TEMPLIST$objList.new)
      } else {
        pList.out <- update.Theme()
        as.ggedit(pList.out)
      }
    }, width = width, height = height)
  })

  shiny::observeEvent(input$SetThemeGrid, {
    pList.out <- update.ThemeGrid()

    output$Plot <- shiny::renderPlot({
      as.ggedit(pList.out)
    }, width = width, height = height)
  })

  simTxt <- shiny::reactive({
    LayerVerbose <- lapply(TEMPLIST$objList.new, function(p) lapply(p$layer, function(item) cloneLayer(l = item, verbose = T, showDefaults = showDefaults)))

    if (is.null(input$activePlot)) {
      aP <- 1
    } else {
      aP <- as.numeric(input$activePlot)
    }

    if (is.null(input$geoms)) {
      l <- 1
    } else {
      l <- which(geom_list(TEMPLIST$obj.new) == input$geoms)
    }

    a <- input$updateElem

    a1 <- input$updateElemPopup

    if (length(l) == 0) {
      l <- 1
    }

    strNew <- strBase <- ""

    if (length(LayerVerbose) > 0) {
      strNew <- LayerVerbose[[aP]][[l]]
    }

    if (length(baseLayerVerbose()) > 0) {
      strBase <- baseLayerVerbose()[[aP]][[l]]
    }

    return(list(Original = strBase, Edited = strNew))
  })

  output$SimPrint <- shiny::renderUI({
    ns <- session$ns

    junk <- ""

    if (length(simTxt()) > 0) {
      junk <- textConnection(utils::capture.output(simTxt()))
    }

    toace <- paste0(readLines(junk), collapse = "\n")

    if (input$viewVerbose %% 2 == 1) {
      if (Sys.info()[1] == "Windows") {
        output$codeout <- shiny::renderText({
          toace
        })

        shiny::verbatimTextOutput("codeout")
      } else {
        shinyAce::aceEditor(
          outputId = "codeout",
          value = toace,
          mode = "r",
          theme = "chrome",
          height = "100px",
          fontSize = 12
        )
      }
    }
  })

  Out <- shiny::eventReactive({
    c(input$sendTheme, input$sendElem)
  }, {
    if (!is.null(input$sendTheme)) {
      if (input$sendTheme > 0) {
        junk1 <- update.Theme()
      }
    }


    if (!is.null(input$sendElem)) {
      if (input$sendElem > 0) {
        junk1 <- update.Layer()
      }
    }


    ggeditOut <- list()

    ggeditOut$UpdatedPlots <- TEMPLIST$objList.new

    class(ggeditOut$UpdatedPlots) <- c("ggedit", class(ggeditOut$UpdatedPlots))

    ggeditOut$UpdatedLayers <- layersListObj(obj = TEMPLIST$objList.new, lbl = names(TEMPLIST$objList.new))

    ggeditOut$UpdatedLayersElements <- layersList(TEMPLIST$objList.new)

    if (verbose) {
      ggeditOut$UpdatedLayerCalls <- lapply(TEMPLIST$objList.new, function(p) lapply(p$layer, function(item) cloneLayer(l = item, verbose = TRUE, showDefaults = showDefaults)))
    }

    names(TEMPLIST$nonLayers) <- names(TEMPLIST$nonLayersTxt) <- names(TEMPLIST$objList.new)

    ggeditOut$updatedScales <- TEMPLIST$nonLayers

    if (verbose) {
      ggeditOut$UpdatedScalesCalls <- TEMPLIST$nonLayersTxt
    }


    if ("themeUpdate" %in% names(TEMPLIST)) {
      ggeditOut$UpdatedThemes <- TEMPLIST$themeUpdate

      if (verbose) {
        ggeditOut$UpdatedThemeCalls <- lapply(names(TEMPLIST$objList.new), function(lp, input) {
          p <- TEMPLIST$objList.new[[lp]]

          if (length(p$theme) > 0) {
            if (!showDefaults) {
              themeBase <- ggplot2::theme_get()

              if (length(TEMPLIST$obj[[lp]]$theme) > 0) {
                themeBase <- themeBase + TEMPLIST$obj[[lp]]$theme
              }

              compare(p$theme, themeBase, verbose = TRUE)
            } else {
              x.theme <- themeFetch(p$theme)

              x <- lapply(names(x.theme), function(item) {
                themeNewVal(x.theme[item], p, input)
              })

              paste0("theme(", paste0(unlist(x), collapse = ","), ")")
            }
          } else {
            c("list()")
          }
        }, input)

        names(ggeditOut$UpdatedThemeCalls) <- names(TEMPLIST$objList.new)
      }
    }

    class(ggeditOut) <- c("ggedit", class(ggeditOut))

    return(ggeditOut)
  })

  return(Out)
}
