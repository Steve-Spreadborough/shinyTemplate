#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @import whereami
#' @import reactlog
#'
#' @noRd

app_server <- function(input, output, session) {

  whereami::cat_where(whereami::whereami())

  # create the dashboard data class object to hold the data
  dash_data <- app_data$new()

  # initiate the variables for the reactives to trigger & watch (note, this is
  # essentially how the R6 class is made "reactive")
  init("date_range", "date_period")

  # run module servers
  mod_pres_select_server("pres_select_ui", dash_data = dash_data)
  mod_date_filter_server("date_filter_ui", dash_data = dash_data)
  mod_explore_data_server("explore_data_ui", dash_data = dash_data)

}
