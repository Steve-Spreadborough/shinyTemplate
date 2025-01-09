

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'
#' @import shiny
#'
#' @noRd

app_ui <- function(request) {

  tagList(

    # Leave this function for adding external resources
    golem_add_external_resources(),

    # navbar in sidebar
    # note: remove code if not using (this causes issues with pages being
    # 'fillable' due to the nav_content function which haven't resolved,
    # see: https://rstudio.github.io/bslib/articles/filling/index.html)
    #page_sidebar(
    #
    #  title = "Shiny template",
    #
    #  sidebar = sidebar(
    #
    #    # Filter summary
    #    bslib::accordion(
    #      #bslib::accordion_panel(
    #        value = "Data summary",
    #        mod_pres_select_ui("pres_select_ui")
    #      #)
    #    ),
    #
    #    # Page navigation
    #    bslib::accordion(
    #      #bslib::accordion_panel(
    #      #  value= "Tabs",
    #        nav_tab(
    #          #span(id = "intro", bs_icon("database"), span("Introduction")),
    #          #span(id = "explore", bs_icon("bar-chart-fill"), span("Explore data")),
    #          span(id = "dev", bs_icon("database"), span("Dev layout"))
    #          )
    #       # )
    #      ),
    #
    #    # 'Master' filters
    #    bslib::accordion(
    #      bslib::accordion_panel(
    #        "Date filter",
    #        mod_date_filter_ui("date_filter_ui")
    #        )
    #      )
    #
    #    ),
    #
    #  # Page modules
    #  nav_content(
    #    #div(id = "intro", mod_intro_ui("intro_1")),
    #    #div(id = "explore", mod_explore_data_ui("explore_data_ui")),
    #    div(id = "dev", mod_dev_layout_ui("dev_layout_ui"))
    #  )
    #)


    # veritcal nav bar
    page_navbar(

      # settings
      title = "ShinyTemplate",
      bg = "#2D89C8",
      inverse = TRUE,

      # sidebar ---------------------------------------------------------------#
      sidebar = sidebar(

        width = 350,

        # Filter summary
        bslib::card(
          #bslib::accordion_panel(
          #value = "Data summary",
          #full_screen = TRUE,
          #card_header("Data: "),
          mod_pres_select_ui("pres_select_ui")
        ),

        # 'Master' filters
        bslib::accordion(
          bslib::accordion_panel(
            "Date filter",
            mod_date_filter_ui("date_filter_ui")
          )
        )

      ),

      # dashboard pages -------------------------------------------------------#
      nav_panel(
        id = "intro",
        title = "Introduction",
        mod_intro_ui("intro1"),
      ),


      nav_panel(
        id = "explore",
        title = "Explore data",
        mod_explore_data_ui("explore_data_1"),
        ),

      nav_panel(
        id = "dev",
        title = "dev layout",
        mod_dev_layout_ui("dev_layout_1"),
        ),

      nav_spacer(),
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
