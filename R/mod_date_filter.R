
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
                choices = c("All" = "all",
                            "Last week" = "1_week",
                            "Last 4 weeks" = "4_week",
                            "Last Month" = "1_month",
                            "Last 3 months" = "3_month",
                            "Last 4 months" = "4_month",
                            "Last 6 months" = "6_month",
                            "Last Year" = "1_year",
                            "Last 18 months" = "18_month",
                            "Last 2 years" = "2_year",
                            "Last 3 years" = "3_year"),
                selected = c("All" = "all"))

  )
}

#' sidebar_date_filter Server Functions
#'
#' @noRd

mod_date_filter_server <- function(id, dash_data){

  moduleServer(id, function(input, output, session){

    ns <- session$ns


    # update the date range when user changes 'date_range'
    observeEvent(input$date_range, {

      # log
      cat_where(where = paste0(whereami(), " - input$date_range"))

      # update dash_data
      dash_data$date_range <- c(input$date_range[1], input$date_range[2])

      # update the date period to 'All', only if daterange is triggered by
      # dateRangeInput and not from the user updating the 'date_period'
      if (dash_data$date_setter == "date_range") {

        updateSelectInput(session = session,
                          inputId = "date_period",
                          label = "Period",
                          choices = c("All" = "all",
                                      "Last week" = "1_week",
                                      "Last 4 weeks" = "4_week",
                                      "Last Month" = "1_month",
                                      "Last 3 months" = "3_month",
                                      "Last 4 months" = "4_month",
                                      "Last 6 months" = "6_month",
                                      "Last Year" = "1_year",
                                      "Last 18 months" = "18_month",
                                      "Last 2 years" = "2_year",
                                      "Last 3 years" = "3_year"),
                          selected = c("All" = "all"))
      }

      dash_data$date_setter <- "date_range"
      trigger("date_range")
    })


    # OUTSTANDING: needs to go back to original dates when select "All"

    # update the date range when user changes 'date_period'
    observeEvent(input$date_period, {

      # log
      cat_where(where = paste0(whereami(), " - input$date_period"))

      # get the dates for the period selected
      dates <- filter_ref_date(df = dash_data$date_ref,
                               period = input$date_period,
                               start = input$date_range[1],
                               end = input$date_range[2])

      # update dash data
      dash_data$date_range <- c(min(dates$date), max(dates$date))

      # trigger that the 'date_range' has been updated to invalidate relevant
      # reactives
      #trigger("date_range")

      # update the date range input so the 2 filters are consistent
      updateDateRangeInput(session = session,
                           inputId = "date_range",
                           label = "Date range",
                           start = min(dates$date),
                           end = max(dates$date))

      # set the date_setter in dash_data to date_period - this is to prevent
      # circular update when ensuring the date_range and date_period align
      dash_data$date_setter <- "date_period"
    })

  })
}
