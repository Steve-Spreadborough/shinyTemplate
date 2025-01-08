

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


#' Filter dates from Date Range & Period UI input
#'
#' Note: currently filters using a reference data frame, needs testing against
#' approach that does it on the fly.
#'
#' @param df data frame with data to filter
#' @param period period input from UI
#' @param start start date
#' @param end end date

filter_ref_date <- function(df, period, start, end) {

  # if date_period is not set, filter by the date range
  if (period == "all" | is.null(period)) {

    output <- df %>%
      filter(date >= start & date <= end)

  } else {

    # work out number of periods
    n_periods <- as.integer(gsub("_.*", "", period))

    # filter data
    if (grepl("week", period)) {

      output <- df %>%
        filter(.data$roll_week >= 1 & .data$roll_week <= n_periods)


    } else if (grepl("month", period)) {

      output <- df %>%
        filter(.data$roll_month >= 1 & .data$roll_month <= n_periods)

    } else if (grepl("year", period)) {

      output <- df %>%
        filter(.data$roll_year >= 1 & .data$roll_year <= n_periods)

    } else {

      warning("looks like date filter not applied...")
    }

  }

  return(output)
}


#' Filter data frame dates
#'
#' Filters dates in data frame using the start and end date from the
#' R6 app_data class object (noting that these are set via the inputs)
#'
#' @param df data frame to filter
#' @param date_f field in data frame that has the date
#' @param r6_data R6 object of class app_data

filter_date <- function(df, date_f, r6_data) {

  #df <- dash_data$stats19
  #date_f <- sym("date")
  #dash_data = dash_data

  output <- df |>
    filter({{ date_f }} >= r6_data$date_range[1] &
             {{ date_f }} <= r6_data$date_range[2])

    return(output)

}


#' Dashboard metrics
#'
#' Function gets metric data. Either calculated within function, or calls
#' another function where more complex.
#'
#' @param metric_ids ID of the metric to be used
#' @param r6_data R6 object that contains the app data (i.e. app_data/dash_data)
#' @param date_f date field to group data by. Default, NA, wont group data by
#' date. Quoted string.
#' @param ... other fields to group data by
#'
#' @importFrom PHEindicatormethods phe_rate
#' @importFrom janitor clean_names
#' @importFrom stats setNames

app_metrics <- function(metric_ids,
                        r6_data,
                        date_f = NA,
                        ...) {

  # get metric details
  metric_details <- r6_data$metric_meta |>
    filter(.data$metric_id %in% metric_ids)


  # check metric id in r6 meta
  if (nrow(metric_details) == 0) {
    stop("metric_ids wrong")
  }

  # get the data
  data <- r6_data$stats19

  # if using date_f, check the field is in the data/date_ref (join to date_ref
  # if need to - note: this needs comparing against calculating on the fly for
  # efficiency)
  if (!is.na(date_f)) {

    if (date_f == "date") {

      date_f <- sym(date_f)

    } else if (date_f %in% colnames(r6_data$date_ref)) {

      date_f <- sym(date_f)

      data <- data |>
        left_join(r6_data$date_ref |>
                    select(.data$date, {{ date_f }}),
                  by = "date")

    }
  }

  # if using date fields in the ..., add them in from date_ref (again, needs
  # assessing if more efficient on the fly than storing date_ref)
  dot_fields <- list(...)

  #dot_fields <- t1(data, {{ x }}, {{ r }})

  date_dot_fields <- dot_fields[dot_fields %in% colnames(r6_data$date_ref) &
                                  dot_fields != "date"]

  if (length(date_dot_fields) > 0) {

    date_fields <- c(as.character(date_dot_fields), "date")

    data <- data |>
      left_join(r6_data$date_ref |>
                  select(all_of(date_fields)),
                by = "date")

  }

  output_data <- data.frame()

  # for each metric, get the data
  for (m_id in metric_details$metric_id) {

    # metric 1: n collisions --------------------------------------------------#

    if (m_id == "n_collisions") {

      # data
      metric_data <- data |>
        filter_date(date_f = date, r6_data = r6_data) |>
        count({{ date_f }}, ..., name = "value") |>
        mutate(metric_id = m_id,
               lowercl = NA,
               uppercl = NA,
               numerator = NA,
               denominator = .data$value)

      output_data <- rbind(output_data, metric_data)

    # metric 2: Rate of casualties per collision ------------------------------#

    } else if (m_id == "rate_casual_per_collision") {

      # data
      metric_data <- data |>
        filter_date(date_f = date, r6_data = r6_data) |>
        group_by({{ date_f }}, ...) |>
        summarise(denominator = n(),
                  numerator = sum(.data$number_of_casualties),
                  .groups = "drop") |>
        phe_rate(x = .data$numerator,
                 n = .data$denominator,
                 multiplier = 100) |>
        mutate(metric_id = m_id) |>
        select(-.data$confidence, -.data$statistic, -.data$method)

      output_data <- rbind(output_data, metric_data)

    }
  }

  output_data <- janitor::clean_names(output_data) |>
    remove_field(.data$na)

  # return data
  return(setNames(list(output_data, metric_details),
                  c("data", "details")))
}


