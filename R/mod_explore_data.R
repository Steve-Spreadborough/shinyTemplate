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
#' @import bslib
#' @import gargoyle

mod_explore_data_ui <- function(id) {

  ns <- NS(id)

  tagList(

    page_sidebar(

      # Sidebar panel for inputs ----
      sidebar = sidebar(

        # set sidebar options
        position = "right",
        width = 420,
        fillable = TRUE,
        fill = TRUE,

        # Inputs - divide into 3 tabs (data, graph & formatting)
        #navset_card_underline(
        navset_bar(

          # Data inputs
          nav_panel("Data",
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

          nav_panel("Graph",
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
                                 selected = "Line")
                    )
          )
        ),

      # main panel to display plot & data
      navset_card_underline(

        title = textOutput(ns("plot_title")),

        # Panel with plot ----
        nav_panel("Plot", plotly::plotlyOutput(ns("plot_explore"))),

        # Panel with table ----
        nav_panel("Table", tableOutput(ns("explore_data")))
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

    # update data & details for plot
    explore_plot_data <- reactive({

      # update plot if 'master filter' date range updated
      gargoyle::watch("date_range")

      # log plot update
      cat_where(where = paste0(whereami(), " - update plot_data"))

      explore_plot_details(input, dash_data)

    })

    # set plot title
    output$plot_title <- renderPrint({


      plot_data <- explore_plot_data()
      #plot_data <- explore_plot_details()

      cat(plot_data$title)
    })

    # set plot subtitle
    output$plot_subtitle <- renderPrint({


      plot_data <- explore_plot_data()
      #plot_data <- explore_plot_details()

      cat(plot_data$subtitle)
    })

    # render table with data
    output$explore_data <- renderTable({

      cat_where(where = paste0(whereami(), " - update table_data"))

      # get the data for the table
      data <- explore_plot_data()
      data$data
    })

    # render plot to explore data
    output$plot_explore <- renderPlotly({

      # update plot if 'master filter' date range updated
      gargoyle::watch("date_range")

      # log plot update
      cat_where(where = paste0(whereami(), " - update plot_explore"))

      # get the data for the plot
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
            aes(ymin = lowercl, ymax = uppercl),
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

