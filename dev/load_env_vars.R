
rm(list = ls())
devtools::document()
devtools::load_all()

# Load the variables available in the environment

dash_data <- app_data$new()

input <- list()

# mod_date_filter inputs -------------------------------------------------------

input$date_range <- c(as.Date("2021-01-01"), as.Date("2022-12-31"))

dash_data$date_range <- input$date_range


# mod_explore_data inputs ------------------------------------------------------

input$metric_id <- c(
  "Number of collisions" = "n_collisions",
  "Rate of casualties per 100 collisions" = "rate_casual_per_collision"
  )[2]

input$plot_group <- c(
  "None" = "none",
  "Metric" = "metric_id",
  "Speed limit" =  "speed_limit",
  "Day of week"  = "day_of_week",
  "Year" = "calender_year"
  )[1]

input$plot_facet <- c(
  "None" = "none",
  "Metric" = "metric_id",
  "Speed limit" =  "speed_limit",
  "Day of week"  = "day_of_week",
  "Year" = "calender_year"
  )[1]

input$plot_x_axis <- c(
  "Date" = "date",
  "Month" = "month_year",
  "Year" = "iso_year",
  "Financial Quarter" = "fq_desc",
  "Rolling 3 months" = "roll_3month",
  "ISO Week" = "iso_year_week"
  )[3]

input$plot_cis <- c("Auto", "Yes", "No")[1]

input$plot_legend <- c("Auto", "Yes", "No")[1]

input$plot_type <- c("Line", "Bar")[1]
