#' @title themeNewVal
#' @description Updates theme with new values from the module UIs.
#' @export
#' @keywords internal
themeNewVal <- function(this, p.new, input) {
  ThemeDefaultClass <- ggedit_opts$get("ThemeDefaultClass")

  out <- list()

  if (themeListDepth(this) == 2) {
    item <- names(this)
    newtxt <- c()

    for (subitem in utils::head(names(this[[1]]), -1)) {
      newval <- input[[paste0("pop", item, subitem)]]

      if (this[[1]][[subitem]]["class"] == "character") {
        newval <- paste0("'", newval, "'")
      } 

      newtxt <- c(newtxt, paste0(this[[1]][[subitem]]["name"], "=", newval))
    }

    out <- c(out, paste0(item, "=", this[[1]][["call"]], "(", paste0(newtxt, collapse = ","), ")"))
  } else {
    item <- names(this)

    for (item1 in names(this[[1]])) {
      newtxt <- c()

      for (subitem in utils::head(names(this[[1]][[item1]]), -1)) {
        check <- input[[paste0("pop", item, item1, subitem)]]

        if (!(check == "" || is.null(check))) {
          subitem.class <- this[[1]][[item1]][[subitem]]["class"] <- "NULL"

          if (this[[1]][[item1]][[subitem]]["class"] %in% c("NULL")) {
            subitem.class <- ThemeDefaultClass$class[ThemeDefaultClass$item == subitem]
          } else {
            subitem.class <- this[[1]][[item1]][[subitem]]["class"]
          }

          if (item != "text" & item1 == "text" & subitem == "size") {
            subitem.class <- "rel"
          } 

          newval <- input[[paste0("pop", item, item1, subitem)]]

          if (subitem.class == "character") {
            newval <- paste0("'", newval, "'")
          }

          if (subitem.class == "rel") {
            newval <- paste0("rel(", newval, ")")
          }

          newtxt <- c(newtxt, paste0(this[[1]][[item1]][[subitem]]["name"], "=", newval))
        }
      }

      if (paste0(newtxt, collapse = ",") != "") {
        if (paste0(paste(item, item1, sep = ".") %in% c("legend.position", "legend.justification"))) {
          if (!grepl("c\\(", newval)) {
            out <- c(out, paste0(paste(item, item1, sep = "."), "=", newval))
          } else {
            out <- c(out, paste0(paste(item, item1, sep = "."), "=", gsub("'", "", newval)))
          }
        } else {
          out <- c(out, paste0(paste(item, item1, sep = "."), "=", this[[1]][[item1]][["call"]], "(", paste0(newtxt, collapse = ","), ")"))
        }
      }
    }
  }

  out <- paste0(unlist(out), collapse = ",")

  return(out)
}
