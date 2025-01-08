

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'
#' @import shiny
#' @import shinydashboardPlus
#' @importFrom shinydashboard dashboardBody menuSubItem tabItem tabItems
#' sidebarMenu menuItem
#'
#' @noRd

app_ui <- function(request) {

  tagList(

    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Create Shiny dashboard
    shinydashboardPlus::dashboardPage(

      # set header
      header = shinydashboardPlus::dashboardHeader(
        title = "shinyTemplate"
      ),


      # Sidebar ---------------------------------------------------------------#

      sidebar = shinydashboardPlus::dashboardSidebar(

        # set width
        width = 300,

        shinydashboard::sidebarMenu(

          id = "sidebarmenu",

          # summary of data selection
          mod_pres_select_ui("pres_select_ui"),


          # page navigation ---------------------------------------------------#

          shinydashboard::menuItem(
            "Introduction",
            tabName = "intro",
            icon = icon("dashboard")
            ),

          shinydashboard::menuItem(
            "Explore data",
            tabName = "explore",
            icon = icon("dashboard")
            ),

          # data filters ------------------------------------------------------#

          # note: these are grouped together in drop downs.

          # date filters
          shinydashboard::menuItem(
            "Filter date",
            tabName = "date_filter",
            shinydashboard::menuSubItem(
              mod_date_filter_ui("date_filter_ui"),
              icon = NULL
              )
            )
          )
        ),


      # dashboard body --------------------------------------------------------#

      body = shinydashboard::dashboardBody(

        shinydashboard::tabItems(
          shinydashboard::tabItem("intro", mod_intro_ui("intro_1")),
          shinydashboard::tabItem("explore", mod_explore_data_ui("explore_data_ui"))
        )
      )
    )
  )
}




#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd

golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "shinyTemplate"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
