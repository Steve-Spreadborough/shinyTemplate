
#' data_selection_pres UI Function
#'
#' @description Present summary of data selected using the filters to make it
#' easier for users to know what data is being presented.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#' @import shinydashboardPlus
#' @importFrom shinydashboard menuItem
#' @import gargoyle
#'
#' @noRd

mod_pres_select_ui <- function(id) {

  ns <- NS(id)


  tagList(

    shinydashboard::menuItem(
      textOutput(ns("tidy_data_summary"))
      )
  )

}

#' data_selection_pres Server Functions
#'
#' @description Present summary of data selected using the filters to make it
#' easier for users to know what data is being presented.
#'
#' @noRd

mod_pres_select_server <- function(id, dash_data){


  moduleServer(id, function(input, output, session){

    ns <- session$ns

    # update the tidy_data_summary when inputs are updated
    output$tidy_data_summary <- renderPrint({

      gargoyle::watch("date_range")

      # write output - add any further summary of filter selections here.
      cat(paste0("Date from ",
                 paste0(dash_data$date_range, collapse = " to ")))
    })

  })
}
