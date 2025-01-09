

# 1. Functions to format navigation bar ---------------------------------------#

# NOTE: only required if implementing navigation bar within the sidebar (which
# then created issues with pages not being 'fillable' which haven't resolved).

#' Java Script for nav_tab function
#'
#' Credit: DavZim https://github.com/rstudio/bslib/issues/585

nav_tab_js <- '
function opentab(tabsetid, tabid) {
    const tabs = document.querySelectorAll("." + tabsetid);
    const contents = document.querySelectorAll("." + tabsetid + "-content");
    // remove bsTab-active and bsTabContent-active classes and hide content
    tabs.forEach(tab => {
      tab.classList.remove("active");
      // set active tab
      if (tab.id == tabid) tab.classList.add("active");
    });
    contents.forEach(c => {
      c.style.display = "none";
      // show content tab
      if (c.id == tabid + "-content") {
        c.style.display = "block";
        $(c).trigger("shown");
      };
    });
}'

#' Nav content
#'
#' Credit: DavZim https://github.com/rstudio/bslib/issues/585
#'
#' @param ids to be added
#' @param container_class to be added
#' @param content_class to be added
#' @param tabsetid to be added
#' @param ... to be added
#'
#' @import shiny
#' @import bslib
#' @import bsicons

nav_content <- function(..., ids = NULL, container_class = NULL,
                        content_class = NULL, tabsetid = "tabSet1") {
  dots <- list(...)

  if (is.null(ids)) ids <- paste0("bsTab-", seq_along(dots))

  if (length(dots) != length(ids))
    stop("ids has to have the same length as the provided tab navigation elements")

  shiny::div(
    as_fill_item(),
    class = container_class,
    lapply(seq_along(dots), function(i) {
      id <- dots[[i]]$attribs$id
      if(is.null(id)) id <- ids[[i]]

      idc <- strsplit(id, "-")[[1]]
      if(idc[length(idc)] != "content") id <- paste0(id, "-content")

      shiny::div(
        class = paste(paste0(tabsetid, "-content"), content_class),
        style = if(i == 1) "display: block;" else "display: none;",
        id = id,
        dots[[i]]
      )
    })
  )
}


#' Nav tab
#'
#' Credit: DavZim https://github.com/rstudio/bslib/issues/585
#'
#' @param ids to be added
#' @param tabsetid to be added
#' @param ... to be added
#'
#' @import shiny
#' @import bslib
#' @import bsicons


nav_tab <- function(..., ids = NULL, tabsetid = "tabSet1") {

  dots <- list(...)

  if (is.null(ids)) ids <- paste0("bsTab-", seq_along(dots))

  if (length(dots) != length(ids))
    stop("ids has to have the same length as the provided tab navigation elements")

  shiny::div(
    as_fill_item(),
    class = "row",
    shiny::div(
      as_fill_item(),
      class = "col-sm-12",
      shiny::tags$ul(
        style = "cursor: pointer;",
        class = "nav nav-pills nav-stacked",
        shiny::singleton(tags$script(shiny::HTML(nav_tab_js))),

        lapply(seq_along(dots), function(i) {
          id <- dots[[i]]$attribs$id
          if(is.null(id)) id <- ids[[i]]

          cl <- paste("nav-item", tabsetid, if (i == 1) "active")

          tags$li(
            class = cl, id = id,
            onclick = sprintf("opentab('%s', '%s');", tabsetid, id),
            tags$a(dots[[i]])
          )
        })
      )
    )
  )
}
