

#' Remove field if in data frame if it exists
#'
#' @param df data frame
#' @param x field to be removed if exists.

remove_field <- function(df, x) {

  if (deparse(substitute(x)) %in% colnames(df)) {
    df %>%
      select(-{{ x }}) %>%
      return(.data)
  } else {
    return(df)
  }
}


# #' Filter dates from Date Range & Period UI input
# #'
# #' Note: currently filters using a reference data frame, needs testing against
# #' approach that does it on the fly.
# #'
# #' @param df data frame with data to filter
# #' @param period period input from UI
# #' @param start start date
# #' @param end end date
#
# filter_ref_date <- function(df, period, start, end) {
#
#   # if date_period is not set, filter by the date range
#   if (period == "all" | is.null(period)) {
#
#     output <- df %>%
#       filter(date >= start & date <= end)
#
#   } else {
#
#     # work out number of periods
#     n_periods <- as.integer(gsub("_.*", "", period))
#
#     # filter data
#     if (grepl("week", period)) {
#
#       output <- df %>%
#         filter(roll_week >= 1 & roll_week <= n_periods)
#
#
#     } else if (grepl("month", period)) {
#
#       output <- df %>%
#         filter(roll_month >= 1 & roll_month <= n_periods)
#
#     } else if (grepl("year", period)) {
#
#       output <- df %>%
#         filter(roll_year >= 1 & roll_year <= n_periods)
#
#     } else {
#
#       warning("looks like date filter not applied...")
#     }
#
#   }
#
#   return(output)
# }
#
#
# #' Filter data frame dates
# #'
# #' Filters dates in data frame using the start and end date from the
# #' R6 app_data class object (noting that these are set via the inputs)
# #'
# #' @param df data frame to filter
# #' @param date_f field in data frame that has the date
# #' @param r6_data R6 object of class app_data
#
# filter_date <- function(df, date_f, r6_data) {
#
#   #df <- dash_data$stats19
#   #date_f <- sym("date")
#   #dash_data = dash_data
#
#   output <- df |>
#     filter({{ date_f }} >= r6_data$date_range[1] &
#              {{ date_f }} <= r6_data$date_range[2])
#
#     return(output)
#
# }
#
#
# #' Dashboard metrics
# #'
# #' Function gets metric data. Either calculated within function, or calls
# #' another function where more complex.
# #'
# #' @param metric_ids ID of the metric to be used
# #' @param r6_data R6 object that contains the app data (i.e. app_data/dash_data)
# #' @param ... other fields to group data by
# #'
# #' @importFrom PHEindicatormethods phe_rate
# #' @importFrom janitor clean_names
# #' @importFrom stats setNames
#
# app_metrics <- function(metric_ids,
#                         r6_data,
#                         ...) {
#
#
#   #metric_ids <- input$metric_id
#   #r6_data <- dash_data
#   #vars <- c("month", "speed_limit", "calender_year")
#
#   # get metric details
#   metric_details <- r6_data$metric_meta |>
#     filter(metric_id %in% metric_ids)
#
#   # check metric id in r6 meta
#   if (nrow(metric_details) == 0) {
#     stop("metric_ids wrong")
#   }
#
#   # get the data
#   data <- r6_data$stats19
#
#   # if using date fields in the ..., add them in from date_ref (again, needs
#   # assessing if more efficient on the fly than storing date_ref)
#   dot_fields <- list(...)
#
#   date_dot_fields <- dot_fields[dot_fields %in% colnames(r6_data$date_ref) &
#                                   dot_fields != "date"]
#
#   if (length(date_dot_fields) > 0) {
#
#     date_fields <- c(as.character(date_dot_fields), "date")
#
#     data <- data |>
#       left_join(r6_data$date_ref |>
#                   select(all_of(date_fields)),
#                 by = "date")
#
#   }
#
#   output_data <- data.frame()
#
#   # for each metric, get the data
#   for (m_id in metric_details$metric_id) {
#
#     # metric 1: n collisions --------------------------------------------------#
#
#     if (m_id == "n_collisions") {
#
#       # data
#       metric_data <- data |>
#         filter_date(date_f = date, r6_data = r6_data) |>
#         mutate(metric_id = m_id) |>
#         count(..., name = "value") |>
#         mutate(metric_id = m_id,
#                lowercl = NA,
#                uppercl = NA,
#                numerator = NA,
#                denominator = value)
#
#       output_data <- rbind(output_data, metric_data)
#
#     # metric 2: Rate of casualties per collision ------------------------------#
#
#     } else if (m_id == "rate_casual_per_collision") {
#
#       # data
#       metric_data <- data |>
#         filter_date(date_f = date, r6_data = r6_data) |>
#         mutate(metric_id = m_id) |>
#         group_by(...) |>
#         summarise(denominator = n(),
#                   numerator = sum(number_of_casualties),
#                   .groups = "drop") |>
#         phe_rate(x = numerator,
#                  n = denominator,
#                  multiplier = 100) |>
#         mutate(metric_id = m_id) |>
#         select(-confidence, -statistic, -method)
#
#       output_data <- rbind(output_data, metric_data)
#
#     }
#   }
#
#   output_data <- janitor::clean_names(output_data) |>
#     remove_field(na)
#
#   # return data
#   return(setNames(list(output_data, metric_details),
#                   c("data", "details")))
# }



#' Calculate details for 'explore data' plot
#'
#' @param ui_inputs the input list from the UI/module.
#' @param mod_data R6 object with dashboard data

explore_plot_details <- function(ui_inputs,
                                 mod_data) {

  # create progress bar
  if (shiny::isRunning()) {

    # Create a Progress object
    progress <- shiny::Progress$new()

    # set number data processes to update on
    n_steps <- 3

    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())

    progress$set(message = "Updating data", value = 0)
  }

  # set group
  if (ui_inputs$plot_group == "none") {
    plot_group <- NA
  } else {
    plot_group <- sym(ui_inputs$plot_group)
  }

  # set facet
  if (ui_inputs$plot_facet %in% c("none", "metric_id")) {
    plot_facet <- NA
  } else {
    plot_facet <- sym(ui_inputs$plot_facet)
  }

  # set x axis - note: if group/facet by year, need to set x axis accordingly
  if (ui_inputs$plot_x_axis %in% c("metric_id", "value")) {

    data_x_axis <- NULL
    plot_x_axis <- sym(ui_inputs$plot_x_axis)
    x_name <- names(ui_inputs$plot_x_axis)

  } else if (ui_inputs$plot_facet %in% c("calender_year") |
             ui_inputs$plot_group %in% c("calender_year")) {

    if (ui_inputs$plot_x_axis == "date") {

      data_x_axis <- sym("day_month")
      plot_x_axis <- sym("day_month")
      x_name <- "Date"

    } else if (ui_inputs$plot_x_axis == "week_start") {

      data_x_axis <- sym("iso_week")
      plot_x_axis <- sym("iso_week")
      x_name <- "Week"

    } else if (ui_inputs$plot_x_axis == "month_year") {

      data_x_axis <- sym("month")
      plot_x_axis <- sym("month")
      x_name <- "Month"

    } else {

      data_x_axis <- sym(ui_inputs$plot_x_axis)
      plot_x_axis <- sym(ui_inputs$plot_x_axis)
      x_name <- names(ui_inputs$plot_x_axis)

    }

  } else {

    data_x_axis <- sym(ui_inputs$plot_x_axis)
    plot_x_axis <- sym(ui_inputs$plot_x_axis)
    x_name <- names(ui_inputs$plot_x_axis)

  }

  # set y axis - note: don't give option to use date on y axis
  if (ui_inputs$plot_y_axis %in% c("metric_id", "value")) {

    data_y_axis <- NULL
    plot_y_axis <- sym(ui_inputs$plot_y_axis)
    y_name <- names(ui_inputs$plot_y_axis)

  } else {

    data_y_axis <- sym(ui_inputs$plot_y_axis)
    plot_y_axis <- sym(ui_inputs$plot_y_axis)
    y_name <- names(ui_inputs$plot_y_axis)

  }

  if (shiny::isRunning()){progress$inc(1/n_steps)}


  # get data
  plot_data <- mod_data$app_metrics(
    metric_ids = ui_inputs$metric_id,
    {{ data_x_axis }},
    {{ data_y_axis }},
    {{ plot_group }},
    {{ plot_facet }}
    )

  if (shiny::isRunning()) {progress$inc(2/n_steps)}

  # add all the details
  plot_data$plot_x_axis <- plot_x_axis
  plot_data$plot_y_axis <- plot_y_axis
  plot_data$x_name <- x_name
  plot_data$y_name <- y_name
  plot_data$plot_group <- plot_group
  plot_data$plot_facet <- plot_facet
  plot_data$title <- paste0(plot_data$details$metric_name, collapse = "/")
  plot_data$subtitle <- paste0(mod_data$date_range, collapse = " to ")

  if (shiny::isRunning()) {progress$inc(3/n_steps)}

  return(plot_data)

}


