#' intro UI Function
#'
#' @description Shiny module containing the introduction details to App.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_intro_ui <- function(id) {

  ns <- NS(id)

  tagList(

    #tags$style(
    #  "#sidebarItemExpanded {
    #        overflow: auto;
    #        max-height: 100vh;
    #    }"
    #),

    fluidPage(
      fluidRow(
        width = 12,

        # 1st column - main intro & info text for dashboard
        column(8,
               style = 'border: 1px solid lightgrey; border-radius: 25px', #overflow-y: scroll',
               br(),
               # title and info button
               div(HTML('<b>Title here</b> '),
                   style = 'display: inline-block;'),
               br(), br(),
               # Text
               p("Text here", style = "font-family: 'times'"),
               br(), br()
               ),

        # 2nd column for plots
        column(4,

               # plot 1
               fluidRow(style = 'border: 1px solid lightgrey; border-radius: 25px; margin-left: 10px; padding-left: 10px;',
                        br(),
                        # Plot title
                        div(HTML('<b>Title here</b> '),
                            style = 'display: inline-block;'),
                        br(), br(),
                        # plot
                        #plotOutput('trend_plot', height = '175px')
                        p("Plot here")
                        ),
               br(),

               # plot2
               fluidRow(style = 'border: 1px solid lightgrey; border-radius: 25px; margin-left: 10px; padding-left: 10px;',
                        br(),
                        # bar plot title and info button
                        div(HTML('<b>Title here</b> '), style = 'display: inline-block;'),
                        br(), br(),
                        # bar plot
                        #plotOutput('bar_plot', height = '175px')
                        p("Plot here")
                        ),
               br(),

               # plot3
               fluidRow(style = 'border: 1px solid lightgrey; border-radius: 25px; margin-left: 10px; padding-left: 10px;',
                        br(),
                        # bar plot title and info button
                        div(HTML('<b>Title here</b> '),
                            style = 'display: inline-block;'),
                        br(), br(),
                        # plot
                        #plotOutput('bar_plot', height = '175px')
                        p("Plot here")
                        )
               )
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
