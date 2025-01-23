
#' sidebar_date_filter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_date_filter_ui <- function(id) {

  ns <- NS(id)

  tagList(

    # date range
    dateRangeInput(ns("date_range"),
                   label = "Date range",
                   start = "2021-01-01",
                   end = "2022-12-31",
                   startview = "year"),

    # pre defined periods
    selectInput(inputId = ns("date_period"),
                label = "Period",
                choices = c("Date range" = "date_range"),
                selected = c("Date range" = "date_range")
                )

  )
}

#' sidebar_date_filter Server Functions
#'
#' @noRd

mod_date_filter_server <- function(id, dash_data){

  moduleServer(id, function(input, output, session){

    ns <- session$ns

    # set the date range to that provided in dash_data to ensure consistency
    # (i.e. if date range set dynamically in R6 & forget to update UI etc)
    # note: this will invalidate some of the following reactives if different
    # from hard coded dates in UI & force them to re-run, but not process heavy.
    updateDateRangeInput(
      inputId = "date_range",
      start = dash_data$data_date_range[1],
      end = dash_data$data_date_range[2]
    )

    # update list of date period options from R6
    updateSelectInput(
      inputId = "date_period",
      choices = dash_data$date_period_options
    )

    # update the date range when user changes 'date_range'
    observeEvent(input$date_range, {

      # log
      cat_where(where = paste0(whereami(), " - input$date_range update"))

      # update dash_data
      dash_data$date_range <- c(input$date_range[1], input$date_range[2])

      # update the date period to 'All', only if daterange is triggered by
      # dateRangeInput and not from the user updating the 'date_period'
      if (dash_data$date_setter == "date_range") {

        updateSelectInput(session = session,
                          inputId = "date_period",
                          label = "Period",
                          choices = dash_data$date_period_options,
                          selected = c("Date range" = "date_range"))
      }

      dash_data$date_setter <- "date_range"

      # trigger that the 'date_range' has been updated to invalidate relevant
      # reactives
      trigger("date_range")
    })

    # update the date range when user changes 'date_period'
    observeEvent(input$date_period, {

      # # log
      cat_where(where = paste0(whereami(), " - input$date_period update"))

      # get the dates for the period selected
      dates <- dash_data$filter_ref_date(
        period = input$date_period,
        start = input$date_range[1],
        end = input$date_range[2]
        )

      # update dash data
      dash_data$date_range <- c(min(dates$date), max(dates$date))

      # update the date range input so the 2 filters are consistent
      updateDateRangeInput(session = session,
                           inputId = "date_range",
                           label = "Date range",
                           start = min(dates$date),
                           end = max(dates$date))

      # set the date_setter in dash_data to date_period - this is to prevent
      # circular update when ensuring the date_range and date_period align
      dash_data$date_setter <- "date_period"

      #print(dash_data$date_period)
    })

  })
}
