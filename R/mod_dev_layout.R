#' dev_layout UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import bslib
#' @importFrom graphics hist
#' @importFrom stats rexp rlnorm runif

mod_dev_layout_ui <- function(id) {

  ns <- NS(id)

  tagList(

    bslib::page_sidebar(

      fillable = TRUE,

      # Sidebar panel for inputs ----
      sidebar = bslib::sidebar(

        # put sidebar on right
        position = "right",

        bslib::navset_card_underline(

          nav_panel("options 1",

                    # Input: Select the random distribution type ----
                    radioButtons(ns("dist"), "Distribution type:",
                                 c("Normal" = "norm",
                                   "Uniform" = "unif",
                                   "Log-normal" = "lnorm",
                                   "Exponential" = "exp"))
          ),

          nav_panel("options 2",
                    # Input: Slider for the number of observations to generate ----
                    sliderInput(ns("n"),
                                "Number of observations:",
                                value = 500,
                                min = 1,
                                max = 1000)

          )
        )
      ),

      # Main panel for displaying outputs ----
      # Output: A tabset that combines three panels ----
      bslib::navset_card_underline(

        title = "Visualizations",
        # Panel with plot ----
        nav_panel("Plot", plotOutput(ns("plot"))),

        # Panel with summary ----
        nav_panel("Summary", verbatimTextOutput(ns("summary"))),

        # Panel with table ----
        nav_panel("Table", tableOutput(ns("table")))
      )
    )

  )
}

#' dev_layout Server Functions
#'
#' @noRd
mod_dev_layout_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Reactive expression to generate the requested distribution ----
    # This is called whenever the inputs change. The output functions
    # defined below then use the value computed from this expression
    d <- reactive({
      dist <- switch(input$dist,
                     norm = rnorm,
                     unif = runif,
                     lnorm = rlnorm,
                     exp = rexp,
                     rnorm)

      dist(input$n)
    })

    # Generate a plot of the data ----
    # Also uses the inputs to build the plot label. Note that the
    # dependencies on the inputs and the data reactive expression are
    # both tracked, and all expressions are called in the sequence
    # implied by the dependency graph.
    output$plot <- renderPlot({
      dist <- input$dist
      n <- input$n

      hist(d(),
           main = paste("r", dist, "(", n, ")", sep = ""),
           col = "#007bc2", border = "white")
    })

    # Generate a summary of the data ----
    output$summary <- renderPrint({
      summary(d())
    })

    # Generate an HTML table view of the data ----
    output$table <- renderTable({
      d()
    })

  })
}

## To be copied in the UI
# mod_dev_layout_ui("dev_layout_1")

## To be copied in the server
# mod_dev_layout_server("dev_layout_1")
