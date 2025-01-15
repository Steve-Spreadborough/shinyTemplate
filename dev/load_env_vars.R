
rm(list = ls())
devtools::document()
devtools::load_all()

# Load the variables available in the environment

dash_data <- app_data$new()

input <- list()

output <- list()

# mod_date_filter inputs -------------------------------------------------------

input$date_range <- c(as.Date("2021-01-01"), as.Date("2022-12-31"))

dash_data$date_range <- input$date_range

input$date_period <- dash_data$date_period_options[1]


# mod_explore_data inputs ------------------------------------------------------

input$metric_id <- setNames(dash_data$metric_meta$metric_id,
                            dash_data$metric_meta$metric_name)

input$plot_group <- dash_data$explore_group_facet[3]

input$plot_facet <- dash_data$explore_group_facet[5]

input$plot_x_axis <- dash_data$explore_x_axis[2]

input$plot_y_axis <-  dash_data$explore_y_axis[2]

input$plot_cis <- c("Auto", "Yes", "No")[3]

input$plot_legend <- c("Auto", "Yes", "No")[1]

input$plot_type <- c("Line", "Bar")[2]

