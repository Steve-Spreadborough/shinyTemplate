#' intro UI Function
#'
#' @description Shiny module containing the introduction details to App.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import bslib

mod_intro_ui <- function(id) {

  ns <- NS(id)

  tagList(

    page_fillable(
      h1("Introduction"),

      layout_columns(

        card(
          full_screen = TRUE,
          p("Text here")
          ),

        layout_columns(
          card(
            card_header("plot 1"),
            full_screen = TRUE,
            p("Plot here")
          ),
          card(
            full_screen = TRUE,
            card_header("plot 2"),
            p("Plot here")
          ),
          card(
            full_screen = TRUE,
            card_header("plot 3"),
            p("Plot here")
          ),
          col_widths = c(12, 12, 12)
        ),

        col_widths = c(8, 4)
        )
    )
    )
}


shinyApp(ui = mod_intro_ui, server = function(input, output) { })


#' intro Server Functions
#'
#' @noRd
mod_intro_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_intro_ui("intro_1")

## To be copied in the server
# mod_intro_server("intro_1")
