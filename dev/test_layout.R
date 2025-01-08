library(shiny)
library(shinydashboardPlus)
library(tidyverse)

# UI ----
ui <- navbarPage(

  #shinyWidgets::useShinydashboard(),

  title = "My App",
  tabPanel(
    "Tab1", icon = icon("home"),
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 2,
          dateRangeInput(inputId   = "date_range",
                         label     = h4("Date Range"),
                         start     = as.Date("2018-01-01"),
                         end       = as.Date("2020-12-31"),
                         min       = as.Date("2018-01-01"),
                         max       = as.Date("2020-12-31"),
                         startview = "year"
          ),
          # create extra vertical space in sidebar (for illustration only)
          HTML(rep('<br>', 30))
        ),

        mainPanel(width = 12,

          # 2nd fluid row for map and plots
          fluidRow(wigth = 12,

            # 1st column for map
            column(10, style = 'border: 1px solid lightgrey; border-radius: 25px',
                   br(),
                   # ntitle and info button
                   div(HTML('<b>Title here</b> '), style = 'display: inline-block; heigh = "100%"'),
                   #uiOutput('sales_map_button', style = 'display: inline-block;'),
                   br(), br(),
                   # Text
                   p("Some long text here"),
                   #plotOutput('sales_map'),
                   br(), br(), br()
            ),

            # 2nd column for plots
            column(2,
                   # fluidRow for sales trend
                   fluidRow(style = 'border: 1px solid lightgrey; border-radius: 25px; margin-left: 10px; padding-left: 10px;',
                            br(),
                            # sales trend title and info button
                            div(HTML('<b>Title here</b> '), style = 'display: inline-block;'),
                            #uiOutput('sales_trend_button', style = 'display: inline-block;'),
                            br(), br(),
                            # trend plot
                            #plotOutput('trend_plot', height = '175px')
                            p("Plot here")
                   ),
                   br(),
                   # fluidRow for bar plot
                   fluidRow(style = 'border: 1px solid lightgrey; border-radius: 25px; margin-left: 10px; padding-left: 10px;',
                            br(),
                            # bar plot title and info button
                            div(HTML('<b>Title here</b> '), style = 'display: inline-block;'),
                            #uiOutput('bar_plot_button', style = 'display: inline-block;'),
                            br(), br(),
                            # bar plot
                            #plotOutput('bar_plot', height = '175px')
                            p("Plot here")
                   ),
                   br(),
                   # fluidRow for bar plot
                   fluidRow(style = 'border: 1px solid lightgrey; border-radius: 25px; margin-left: 10px; padding-left: 10px;',
                            br(),
                            # bar plot title and info button
                            div(HTML('<b>Title here</b> '), style = 'display: inline-block;'),
                            #uiOutput('bar_plot_button', style = 'display: inline-block;'),
                            br(), br(),
                            # bar plot
                            #plotOutput('bar_plot', height = '175px')
                            p("Plot here")
                   )
            )
          )
        )
      )
    )
  )
)

# Server ----
server <- function(input, output) {

  # Box 1
  output$box_1 <- shinydashboardPlus::renderValueBox({
    valueBox(5, "box1", color = "green"
    )
  })

  # Box 2
  output$box_2 <- renderValueBox({
    valueBox(10, "box2", color = "blue"
    )
  })

  # Box 3
  output$box_3 <- renderValueBox({
    valueBox(15, "box1", color = "purple"
    )
  })

  # Box 4
  output$box_4 <- renderValueBox({
    valueBox(20, "box1", color = "orange"
    )
  })

  # sales map button
  output$sales_map_button <- renderUI({
    actionButton('salesMapButton', NULL, icon = icon('info'), style = 'border-radius: 50%;')
  })

  # sales map
  output$sales_map = renderPlot({
    ggplot(mtcars, aes(x = disp, y = mpg)) + geom_point()
  })

  # sales trend button
  output$sales_trend_button <- renderUI({
    actionButton('salesTrendButton', NULL, icon = icon('info'), style = 'border-radius: 50%;')
  })

  # sales trend plot
  output$trend_plot = renderPlot({
    ggplot(mtcars, aes(x = disp, y = mpg, group = 'cyl')) + geom_line()
  })

  # bar plot button
  output$bar_plot_button <- renderUI({
    actionButton('barPlotButton', NULL, icon = icon('info'), style = 'border-radius: 50%;')
  })

  # bar plot
  output$bar_plot = renderPlot({
    ggplot(count(mtcars, cyl), aes(x = cyl, y = n)) + geom_bar(stat = 'identity')
  })
}

# Run the application
shinyApp(ui = ui, server = server)
