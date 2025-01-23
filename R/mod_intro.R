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
          p("This is an example Shiny dashboard that uses modules, golem,
            bslibs & R6 class object to share data between modules."),
          p("The 'Explore data' tab contains a plot with options of different
            metrics and settings, including options to group & fact the data
            by and fields to use in the x & y axis. This essentially acts in a
            similar way to a pivot table."),
          p("p creates a paragraph of text."),
          p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
          strong("strong() makes bold text."),
          em("em() creates italicized (i.e, emphasized) text."),
          br(),
          code("code displays your text similar to computer code"),
          div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
          br(),
          p("span does the same thing as div, but it works with",
            span("groups of words", style = "color:blue"),
            "that appear inside a paragraph."),
          p("tags$ul and tags$li can be used to write bullet points:"),
          tags$ul(
            tags$li("First list item"),
            tags$li("Second list item"),
            tags$li("Third list item")
          )
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
