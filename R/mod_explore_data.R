#' explore_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import ggplot2
#' @importFrom plotly ggplotly renderPlotly plotlyOutput
#' @importFrom DT renderDT
#' @import dplyr

mod_explore_data_ui <- function(id) {

  ns <- NS(id)

  # make plot fit entire screen
  plot_style <- paste0("#",ns("plot_explore"),
                       "{height: calc(100vh - 220px) !important;}")

  tagList(

    fillPage(

      tags$style(type = "text/css", plot_style),

      titlePanel("Explore data"),

      sidebarLayout(

        # data exploration plot
        mainPanel = mainPanel(

          fillRow(
            #flex = c(7, 3),
            #fillCol(
              #width = "98%",
              column(12,
                     style = 'border: 1px solid lightgrey; border-radius: 25px', #overflow-y: scroll',
                     br(),
                     # title and info button
                     #div(HTML('<b>Main plot</b> '),
                    #     style = 'display: inline-block;'),
                     #br(), br(),
                    plotly::plotlyOutput(ns("plot_explore")),
                     br(), br()
                     ),
              #)
          )
        ),

        # set the extra controls to the right
        position = "right",

        # extra controls for data exploration plot
        sidebarPanel = sidebarPanel(
          tabsetPanel(
          tabPanel("Data",
                   #uiOutput(ns("metric_select")),
                   selectInput(inputId = ns("metric_id"),
                               label = "Metric",
                               multiple = TRUE,
                               choices = c(
                                 "Number of collisions" = "n_collisions",
                                 "Rate of casualties per 100 collisions" = "rate_casual_per_collision"
                               ),
                               selected = "n_collisions"),
                   selectInput(inputId = ns("plot_x_axis"),
                               label = "x axis",
                               choices = c(
                                 "Date" = "date",
                                 "Week" = "week_start",
                                 "Month" = "month_year",
                                 "Year" = "calender_year",
                                 "Financial Quarter" = "fq_desc",
                                 "Rolling 3 months" = "roll_3month",
                                 "ISO Week" = "iso_year_week",
                                 "Metric" = "metric_id",
                                 "Value" = "value"
                                 ),
                               selected = "date"),
                   selectInput(inputId = ns("plot_y_axis"),
                               label = "y axis",
                               choices = c(
                                 "Value" = "value",
                                 "Metric" = "metric_id"
                               ),
                               selected = "date"),
                   selectInput(inputId = ns("plot_group"),
                               label = "Group data",
                               choices = c(
                                 "None" = "none",
                                 "Metric" = "metric_id",
                                 "Accident severity" = "accident_severity",
                                 "Police force" = "police_force",
                                 "Road speed limit" =  "speed_limit",
                                 "Day of week"  = "day_of_week",
                                 "Year" = "calender_year"
                               ),
                               selected = "None"),
                   selectInput(inputId = ns("plot_facet"),
                               label = "Facet plot",
                               choices = c(
                                 "None" = "none",
                                 "Metric" = "metric_id",
                                 "Accident severity" = "accident_severity",
                                 "Police force" = "police_force",
                                 "Road speed limit" =  "speed_limit",
                                 "Day of week"  = "day_of_week",
                                 "Year" = "calender_year"
                               ),
                               selected = "None")
                   ),
          tabPanel("Graph",
                   radioButtons(inputId = ns("plot_cis"),
                                label = "Confidence intervals",
                                choices = c("Auto", "Yes", "No"),
                                selected = "Auto"),
                   radioButtons(inputId = ns("plot_legend"),
                                label = "Legend",
                                choices = c("Auto", "Yes", "No"),
                                selected = "Auto"),
                   radioButtons(inputId = ns("plot_type"),
                                label = "Plot type",
                                choices = c("Line", "Bar"),
                                selected = "Line"),

          ),

          tabPanel("Other settings 2")
          )
          )
      )
    )
  )
}

#' explore_data Server Functions
#'
#' @noRd


mod_explore_data_server <- function(id, dash_data){

  #dash_data <- app_data$new()

  moduleServer(id, function(input, output, session){

    ns <- session$ns

    # code to set metric id drop down options from metrics available in dash_data
    # note: doing it like this means the plot below refreshes twice initially,
    # and likely more inefficient than just listing all the metric ids in UI
    # above as has to go back to the server & the R6 to get the details (hence
    # not used).

    #output$metric_select <- renderUI({

      ## log
      #cat_where(where = paste0(whereami(), " - output$metric_select"))

      #metric_ids <- dash_data$metric_meta$metric_id
      #names(metric_ids) <- dash_data$metric_meta$metric_name

      ## output
      #updateSelectInput(session,
      #                  inputId = ns("metric_select"),
      #            #label = "Metric",
      #            choices = metric_ids,
      #            selected = metric_ids[1])

     #})

    # function to get data for explore plot
    explore_plot_details <- function() {

      # set group
      if (input$plot_group == "none") {
        plot_group <- NA
      } else {
        plot_group <- sym(input$plot_group)
      }

      # set facet
      if (input$plot_facet %in% c("none", "metric_id")) {
        plot_facet <- NA
      } else {
        plot_facet <- sym(input$plot_facet)
      }

      # set x axis - note: if group/facet by year, need to set x axis accordingly
      if (input$plot_x_axis %in% c("metric_id", "value")) {

        data_x_axis <- NULL
        plot_x_axis <- sym(input$plot_x_axis)
        x_name <- names(input$plot_x_axis)

      } else if (input$plot_facet %in% c("calender_year") |
                 input$plot_group %in% c("calender_year")) {

        if (input$plot_x_axis == "date") {

          data_x_axis <- "day_month"
          plot_x_axis <- "day_month"
          x_name <- "Date"

        } else if (input$plot_x_axis == "week_start") {

          data_x_axis <- "iso_week"
          plot_x_axis <- "iso_week"
          x_name <- "Week"

        } else if (input$plot_x_axis == "month_year") {

          data_x_axis <- "month"
          plot_x_axis <- "month"
          x_name <- "Month"

        } else {

          data_x_axis <- sym(input$plot_x_axis)
          plot_x_axis <- sym(input$plot_x_axis)
          x_name <- names(input$plot_x_axis)

        }

      } else {

        data_x_axis <- sym(input$plot_x_axis)
        plot_x_axis <- sym(input$plot_x_axis)
        x_name <- names(input$plot_x_axis)

      }

      # set y axis - note: don't give option to use date on y axis
      if (input$plot_y_axis %in% c("metric_id", "value")) {

        data_y_axis <- NULL
        plot_y_axis <- sym(input$plot_y_axis)
        y_name <- names(input$plot_y_axis)

      } else {

        data_y_axis <- sym(input$plot_y_axis)
        plot_y_axis <- sym(input$plot_y_axis)
        y_name <- names(input$plot_y_axis)

      }


      # get data
      plot_data <- app_metrics(metric_ids = input$metric_id,
                               r6_data = dash_data,
                               {{ data_x_axis }},
                               {{ data_y_axis }},
                               {{ plot_group }},
                               {{ plot_facet }})

      # add all the details
      plot_data$plot_x_axis <- plot_x_axis
      plot_data$plot_y_axis <- plot_y_axis
      plot_data$x_name <- x_name
      plot_data$y_name <- y_name
      plot_data$plot_group <- plot_group
      plot_data$plot_facet <- plot_facet
      plot_data$title <- paste0(plot_data$details$metric_name, collapse = "/")
      plot_data$subtitle <- paste0(dash_data$date_range, collapse = " to ")

      return(plot_data)

    }

    # update data & details for plot
    explore_plot_data <- reactive({

      # update plot if 'master filter' date range updated
      gargoyle::watch("date_range")

      # log plot update
      cat_where(where = paste0(whereami(), " - update plot_data"))

      explore_plot_details()

    })


    # rend table with data
    output$explore_data <- DT::renderDT({
      explore_plot_data$data
    })

    # render plot to explore data
    output$plot_explore <- renderPlotly({

      # update plot if 'master filter' date range updated
      gargoyle::watch("date_range")

      # log plot update
      cat_where(where = paste0(whereami(), " - update plot_explore"))

      ## set group
      #if (input$plot_group == "none") {
      #  plot_group <- NA
      #} else {
      #  plot_group <- sym(input$plot_group)
      #}
#
      ## set facet
      #if (input$plot_facet %in% c("none", "metric_id")) {
      #  plot_facet <- NA
      #} else {
      #  plot_facet <- sym(input$plot_facet)
      #}
#
      ## set x axis - note: if group/facet by year, need to set x axis accordingly
      #if (input$plot_x_axis %in% c("metric_id", "value")) {
#
      #  data_x_axis <- NULL
      #  plot_x_axis <- sym(input$plot_x_axis)
      #  x_name <- names(input$plot_x_axis)
#
      #} else if (input$plot_facet %in% c("calender_year") |
      #           input$plot_group %in% c("calender_year")) {
#
      #  if (input$plot_x_axis == "date") {
#
      #    data_x_axis <- "day_month"
      #    plot_x_axis <- "day_month"
      #    x_name <- "Date"
#
      #  } else if (input$plot_x_axis == "week_start") {
#
      #    data_x_axis <- "iso_week"
      #    plot_x_axis <- "iso_week"
      #    x_name <- "Week"
#
      #  } else if (input$plot_x_axis == "month_year") {
#
      #    data_x_axis <- "month"
      #    plot_x_axis <- "month"
      #    x_name <- "Month"
#
      #  } else {
#
      #    data_x_axis <- sym(input$plot_x_axis)
      #    plot_x_axis <- sym(input$plot_x_axis)
      #    x_name <- names(input$plot_x_axis)
#
      #  }
#
      #} else {
#
      #  data_x_axis <- sym(input$plot_x_axis)
      #  plot_x_axis <- sym(input$plot_x_axis)
      #  x_name <- names(input$plot_x_axis)
#
      #}
#
      ## set y axis - note: don't give option to use date on y axis
      #if (input$plot_y_axis %in% c("metric_id", "value")) {
#
      #  data_y_axis <- NULL
      #  plot_y_axis <- sym(input$plot_y_axis)
      #  y_name <- names(input$plot_y_axis)
#
      #} else {
#
      #  data_y_axis <- sym(input$plot_y_axis)
      #  plot_y_axis <- sym(input$plot_y_axis)
      #  y_name <- names(input$plot_y_axis)
#
      #}
#
#
      ## get data
      #plot_data <- app_metrics(metric_ids = input$metric_id,
      #                         r6_data = dash_data,
      #                         {{ data_x_axis }},
      #                         {{ data_y_axis }},
      #                         {{ plot_group }},
      #                         {{ plot_facet }})
#
#

      plot_data <- explore_plot_data()

      #plot_data <- explore_plot_details()

      # if using group, make sure its a factor
      if (input$plot_group != "none") {

        plot_data$data[[input$plot_group]] <- factor(
          plot_data$data[[input$plot_group]],
          levels = sort(unique(plot_data$data[[input$plot_group]]))
        )
      }

      # if using facet, make sure its a factor
      if (input$plot_facet != "none") {

        plot_data$data[[input$plot_facet]] <- factor(
          plot_data$data[[input$plot_facet]],
          levels = sort(unique(plot_data$data[[input$plot_facet]]))
        )
      }

      # get fields for plot
      plot_x_axis <- plot_data$plot_x_axis
      plot_y_axis <- plot_data$plot_y_axis
      plot_group <- plot_data$plot_group
      plot_facet <- plot_data$plot_facet

      # create the plot
      plot <- plot_data$data |>
        ggplot(aes(x = {{ plot_x_axis }},
                   y = {{ plot_y_axis }},
                   group = {{ plot_group }},
                   colour = {{ plot_group }}))

      # set graph type
      if (input$plot_type == "Line") {

        plot <- plot +
          geom_line() +
          geom_point()

      } else if (input$plot_type == "Bar") {

        plot <- plot +
          geom_bar(stat = "identity", position = position_dodge())
          #geom_bar(stat = "identity")

      }



      # add confidence intervals
      if (input$plot_cis %in% c("Auto", "Yes") &
          (TRUE %in% grepl("rate", tolower(plot_data$details$value_type)))) {

        plot <- plot +
          geom_errorbar(
            aes(ymin = .data$lowercl, ymax = .data$uppercl),
            width = 0.2,
            position = position_dodge(0.9)
            )

      }

      # add facet
      if (input$plot_facet != "none") {

        if (input$plot_facet == "metric_id") {

          plot <- plot +
            facet_wrap(input$plot_facet, scales = "free")

        } else {

          plot <- plot +
            facet_wrap(input$plot_facet)

        }
      }

      # set legend
      if ((input$plot_legend == "Auto" & !is.symbol(plot_group)) |
          input$plot_legend == "No") {

        plot <- plot +
          theme(legend.position = "none")

      }

      # set labels
      plot <- plot  +
        labs(
          x = plot_data$x_name
        )

      ggplotly(plot)

      })

  })
}

