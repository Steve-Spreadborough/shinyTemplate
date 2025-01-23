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

# TO DO:
# 1. Add indication of incomplete data - both "ends", in terms of lastest time
#    period not complete but also starting, i.e. filtered for "current month"
#    and then grouped by week, means first week might be incomplete...

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
                    selectInput(
                      inputId = ns("metric_id"),
                      label = "Metric",
                      multiple = TRUE,
                      choices = c("Number of collisions" = "n_collisions"),
                      selected = "Number of collisions"
                      ),
                    selectInput(
                      inputId = ns("plot_x_axis"),
                      label = "x axis",
                      choices = c("Date" = "date"),
                      selected = "Date"
                      ),
                    selectInput(
                      inputId = ns("plot_y_axis"),
                      label = "y axis",
                      choices = c("Value" = "value"),
                      selected = "Value"
                      ),
                    selectInput(
                      inputId = ns("plot_group"),
                      label = "Group data",
                      choices = c("None" = "none"),
                      selected = "None"
                      ),
                    selectInput(
                      inputId = ns("plot_facet"),
                      label = "Facet plot",
                      choices = c("None" = "none"),
                      selected = "None")
                    ),

          # Graph formatting settings
          nav_panel("Graph",
                    radioButtons(
                      inputId = ns("plot_cis"),
                      label = "Confidence intervals",
                      choices = c("Auto", "Yes", "No"),
                      selected = "Auto"
                      ),
                    radioButtons(
                      inputId = ns("plot_legend"),
                      label = "Legend",
                      choices = c("Auto", "Yes", "No"),
                      selected = "Auto"
                      ),
                    radioButtons(
                      inputId = ns("plot_type"),
                      label = "Plot type",
                      choices = c("Line", "Bar"),
                      selected = "Line"
                      )
                    )
          )
        ),

      # main panel to display plot & data
      navset_card_underline(

        # Panel with plot ----
        nav_panel("Plot", plotly::plotlyOutput(ns("plot_explore"))),

        # Panel with table ----
        nav_panel("Table", tableOutput(ns("explore_data"))),

        # other settings
        title = textOutput(ns("plot_title")),
        full_screen = TRUE
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

    # Add the options on the select inputs
    # note:
    #  1. this is to avoid having to type out the same list of options in
    #     multiple places (e.g. unit tests, other modules using same options)
    #  2. ensure 'selected' option the same as in UI above to avoid invalidating
    #     the further outputs (and making it re-run when dashboard is loaded)

    # metrics
    updateSelectInput(
      inputId = "metric_id",
      choices = setNames(dash_data$metric_meta$metric_id,
                         dash_data$metric_meta$metric_name),
      selected = "n_collisions"
    )

    # x axis
    updateSelectInput(
      inputId = "plot_x_axis",
      choices = dash_data$explore_x_axis,
      selected = "date"
    )

    # y axis
    updateSelectInput(
      inputId = "plot_y_axis",
      choices = dash_data$explore_y_axis,
      selected = "value"
    )

    # group variable
    updateSelectInput(
      inputId = "plot_group",
      choices = dash_data$explore_group_facet,
      selected = "none"
    )

    # facet variable
    updateSelectInput(
      inputId = "plot_facet",
      choices = dash_data$explore_group_facet,
      selected = "none"
    )

    # update data & details for plot
    explore_plot_data <- reactive({

      # update plot if 'master filter' date range updated
      gargoyle::watch("date_range")

      # log plot update
      cat_where(where = paste0(whereami(), " - update plot_data"))

      explore_plot_details(ui_inputs = input, mod_data = dash_data)

    })

    # set plot title
    output$plot_title <- renderPrint({


      plot_data <- explore_plot_data()
      #plot_data <- explore_plot_details(ui_inputs = input, mod_data = dash_data)

      cat(plot_data$title)
    })

    # set plot subtitle
    output$plot_subtitle <- renderPrint({


      plot_data <- explore_plot_data()
      #plot_data <- explore_plot_details(ui_inputs = input, mod_data = dash_data)

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

      #plot_data <- explore_plot_details(ui_inputs = input, mod_data = dash_data)

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
        dash_data$set_factor_levels() |>
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

