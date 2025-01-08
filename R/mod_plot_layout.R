# #' explore_data UI Function
# #'
# #' @description A shiny Module.
# #'
# #' @param id,input,output,session Internal parameters for {shiny}.
# #'
# #' @noRd
# #'
# #' @importFrom shiny NS tagList
#
# mod_plot_layout_ui <- function(id) {
#
#   ns <- NS(id)
#
#   plot1_style <- paste0("#",ns("plotLeft"),
#                         "{height: calc(100vh - 200px) !important;}")
#
#   #group_style <- "#group {height: calc(100vh - 40px) !important;}"
#   #facet_style <- "#facet {height: calc(100vh - 40px) !important;}"
#
#
#   tagList(
#
#     fillPage(
#
#       tags$style(type = "text/css", plot1_style),
#       #tags$style(type = "text/css", group_style),
#       #tags$style(type = "text/css", facet_style),
#
#       fillRow(
#         flex = c(7, 3),
#
#         fillCol(
#           width = "98%",
#           column(12,
#                  style = 'border: 1px solid lightgrey; border-radius: 25px', #overflow-y: scroll',
#                  br(),
#                  # title and info button
#                  div(HTML('<b>Main plot</b> '),
#                      style = 'display: inline-block;'),
#                  br(), br(),
#                  plotOutput(ns("plotLeft")),
#                  br(), br()
#           ),
#
#
#
#
#           #plotOutput(ns("plotLeft")),
#         ),
#
#         fillCol(width = "98%",
#                 flex = c(1, 2),
#           #plotOutput(ns("plotTopRight")),
#           #plotOutput(ns("plotBottomRight"))
#
#           fillRow(
#             #hieght = "98%",
#             column(12,
#                    style = 'border: 1px solid lightgrey; border-radius: 25px', #overflow-y: scroll',
#                    br(),
#                    # title and info button
#                    div(HTML('<b>Plot 2</b> '),
#                        style = 'display: inline-block;'),
#                    br(), br(),
#                    plotOutput(ns("plotTopRight")),
#                    br(), br()
#                    )
#             ),
#
#           br(),
#
#           fillRow(
#               #hieght = "98%",
#               column(12,
#                      style = 'border: 1px solid lightgrey; border-radius: 25px', #overflow-y: scroll',
#                      br(),
#                      # title and info button
#                      div(HTML('<b>Plot 3</b> '),
#                          style = 'display: inline-block;'),
#                      br(), br(),
#                      plotOutput(ns("plotBottomRight")),
#                      br(), br()
#                      )
#               )
#
#
#
#           ## plot 1
#           #fillRow(style = 'border: 1px solid lightgrey; border-radius: 25px; margin-left: 10px; padding-left: 10px;',
#           #         br(),
#           #         # Plot title
#           #         div(HTML('<b>Title here</b> '),
#           #             style = 'display: inline-block;'),
#           #         br(), br(),
#           #         # plot
#           #         #plotOutput('trend_plot', height = '175px')
#           #         p("Plot here")
#           #),
#           #br(),
# #
#           ## plot2
#           #fillRow(style = 'border: 1px solid lightgrey; border-radius: 25px; margin-left: 10px; padding-left: 10px;',
#           #         br(),
#           #         # bar plot title and info button
#           #         div(HTML('<b>Title here</b> '), style = 'display: inline-block;'),
#           #         br(), br(),
#           #         # bar plot
#           #         #plotOutput('bar_plot', height = '175px')
#           #         p("Plot here")
#           #),
#           #br()
#           )
#         )
#     )
#
#   )
# }
#
# #' explore_data Server Functions
# #'
# #' @noRd
#
#
# mod_plot_layout_server <- function(id, dash_data ){
#
#   moduleServer(id, function(input, output, session){
#
#     ns <- session$ns
#
#     output$plotLeft <- renderPlot(plot(cars))
#     output$plotTopRight <- renderPlot(plot(pressure))
#     output$plotBottomRight <- renderPlot(plot(AirPassengers))
#
#   })
# }

