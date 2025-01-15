

#' @title R6 Class to get & hold dashboard data
#'
#' @description
#' Create R6 class object to read in data for App.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @return
#' Object that contains data required for App & related methods.
#'
#' @import glue
#' @import lubridate
#' @import R6
#' @import dplyr
#' @import stringr
#' @import stats19
#' @importFrom shiny Progress
#'
#' @export

app_data <- R6Class(

  "app_data",
  public = list(

    #' @field date_range variable to hold start and end date from 'master'
    #' dateRangeInput. Note: this is the 'single source of truth' date range
    #' to be used across the App, and is set via dateRangeInput & updated
    #' from 'date_period' input.
    date_range = c(),

    #' @field data_date_range the date range available from the data set. Note
    #' this is set when initialised and should not be edited. Values used in
    #' the initial 'master' dateRangeInput & when 'period' set to 'All'. Also
    #' used to set limit on dates for 'master' dateRangeInput (TO BE DONE!).
    data_date_range = c(),

    #' @field date_period_options variable that holds list of options for the
    #' date_period input.
    date_period_options = NULL,

    #' @field explore_x_axis options for x axis in explore plot.
    explore_x_axis = NULL,

    #' @field explore_y_axis options for y axis in explore plot.
    explore_y_axis = NULL,

    #' @field explore_group_facet option for group/facet in explore plot.
    explore_group_facet = NULL,

    #' @field date_setter variable indicator if `date_range` has been set via
    #' dateRangeInput or the 'date_period' input. Note: this is required to
    #' prevent circular behavior in the UI, as updating 'date_period' input
    #' also has to update the dateRangeInput input for consistency (see the
    #' details [here](https://github.com/rstudio/shiny/issues/2324))
    date_setter = "date_range",

    #' @field date_ref data frame with reference dates. Note: this is
    #' experimental, needs testing to see if more efficient to calculate these
    #' dates as required on the fly rather than holding as a data frame (used
    #' for grouping time periods as well as filtering).
    date_ref = NULL,

    #' @field stats19 stats19 data set from stats19 package
    stats19 = NULL,

    #' @field metric_meta data frame contain meta data for metrics
    metric_meta = NULL,

    #' Method 1: initialize object
    #'
    #' @description
    #' Create R6 class object to get data for App.
    initialize = function() {

      # give indication downloading data
      if (shiny::isRunning()) {

        # Create a Progress object
        progress <- shiny::Progress$new()

        # set number data processes to update on
        n_data <- 5

        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())

        progress$set(message = "Loading data", value = 0)
      }


      # 1. Set variables & list of input options to be used in UI -------------#

      # hard coded start & end date (consider deriving this from data set being
      # read in if needs to be dynamic).
      start_date <- as.Date("2021-01-01")
      end_date <- as.Date("2022-12-31")

      # set the data date range - note this is used to set the initial 'master'
      # dateRangeInput values & to set the dateRangeInput values when 'period'
      # is set to 'All'.
      self$data_date_range[1] <- start_date
      self$data_date_range[2] <- end_date

      # set date_period_options (used in mod_date_filter)
      self$date_period_options <- c(
        "All" = "all",
        "Current week" = "0_week_current",
        "Current month" = "0_month_current",
        "Current financial quarter" = "0_f_quarter_current",
        "Current 12 months" = "11_month_current",
        "Current financial year" = "0_f_year_current",
        "Current 24 months" = "23_month_current",
        "Last week" = "1_week",
        "Last month" = "1_month",
        "Last financial quarter" = "1_f_quarter",
        "Last 12 months" = "12_month",
        "Last 24 months" = "24_month"
      )

      # set explore_x_axis (used in mod_explore_data)
      self$explore_x_axis <- c(
        "Date" = "date",
        "Week" = "week_start",
        "Month" = "month_year",
        "Year" = "calender_year",
        "Financial Quarter" = "fq_desc",
        "Rolling 3 months" = "roll_3month",
        "ISO Week" = "iso_year_week",
        "Metric" = "metric_id",
        "Value" = "value"
      )

      # set explore_y_axis (used in mod_explore_data)
      self$explore_y_axis <- c(
        "Value" = "value",
        "Metric" = "metric_id"
      )

      # set explore_group_facet (used in mod_explore_data)
      self$explore_group_facet <- c(
        "None" = "none",
        "Metric" = "metric_id",
        "Accident severity" = "accident_severity",
        "Police force" = "police_force",
        "Road speed limit" =  "speed_limit",
        "Day of week"  = "day_of_week",
        "Year" = "calender_year"
      )

      # update data download
      if (shiny::isRunning()) {progress$inc(1/n_data)}

      # 2. Read in/create reference data --------------------------------------#

      # create date reference table (note: needs comparison to calculating
      # required field the fly in terms of efficiency)
      date_ref <- data.frame(date = seq(start_date, end_date, by = 'days')) %>%
        mutate(month_year = floor_date(date, unit = "month"),
               month = month.abb[month(date)],
               day_month = paste0(day(date), " - ", month),
               calender_year = year(date),
               iso_week = lubridate::isoweek(date),
               iso_year = lubridate::isoyear(date),
               f_year = case_when(month(date) %in% 4:12 ~ paste0(year(date) - 2000, "/", year(date) - 1999),
                                  TRUE ~  paste0(year(date) - 2001, "/", year(date) - 2000)),
               f_quarter = case_when(month(date) %in% 4:6 ~ 1 ,
                                     month(date) %in% 7:9 ~ 2,
                                     month(date) %in% 10:12 ~ 3,
                                     month(date) %in% 1:3 ~ 4),
               fq_desc = paste0(f_year, " - Q", f_quarter),
               week_start = floor_date(date,
                                       unit = "week",
                                       week_start = getOption("lubridate.week.start", 1)),
               week_end = week_start + 6,
               iso_year_week = paste0(iso_year, "-", iso_week)
        ) %>%
        group_by(f_quarter) %>%
        mutate(fq_date = min(date)) %>%
        ungroup() %>%
        group_by(iso_week) %>%
        mutate(iso_week_date = min(date)) %>%
        ungroup()

      # add in rolling time periods
      self$date_ref <- date_ref %>%
        left_join(date_ref %>%
                    select(month_year) %>%
                    unique() %>%
                    mutate(roll_month = (n()-1):0),
                  by = "month_year") %>%
        left_join(date_ref %>%
                    select(week_start)  %>%
                    unique() %>%
                    mutate(roll_week = (n()-1):0),
                  by = "week_start") %>%
        left_join(date_ref %>%
                    select(fq_desc)  %>%
                    unique() %>%
                    mutate(roll_f_quarter = (n()-1):0),
                  by = "fq_desc") %>%
        left_join(date_ref %>%
                    select(f_year)  %>%
                    unique() %>%
                    mutate(roll_f_year = (n()-1):0),
                  by = "f_year") %>%
        mutate(
          roll_4week = as.integer(floor((roll_week+3)/4)),
          roll_3month = as.integer(floor((roll_month+2)/3)),
          roll_4month = as.integer(floor((roll_month+3)/4)),
          #roll_6month = as.integer(floor((roll_month+5)/6)),
          #roll_18month = as.integer(floor((roll_month+17)/18)),
          roll_year = as.integer(floor((roll_month+11)/12)),
          all_dates = 1)

      # update data download
      if (shiny::isRunning()) {progress$inc(2/n_data)}

      # log - make it red as should only happen once
      cat_where(where = paste0(whereami(), " - created date_ref"), color = "red")

      # meta data - note: probably best to have as a table somewhere to be
      # read in (e.g. pinned on posit connect/sql db etc). Written out here
      # as example.
      self$metric_meta <- matrix(
        c(
          "n_collisions",
            "Number of collisions",
            "Number of collisisons recorded in STATS19 data for time period specified",
            "Count",
          "rate_casual_per_collision",
            "Rate of casualties per 100 collisions",
            "Rate of casualities per 100 collisions as recorded in STATS19 data for time period specified",
            "Rate per 100"
          ),
        ncol = 4,
        byrow = TRUE
        ) |>
        as.data.frame() |>
        rename(metric_id = V1,
               metric_name = V2,
               metric_detail = V3,
               value_type = V4)


      # read in stats19 2021 data
      suppressMessages(
        suppressWarnings(
          stats19_2021 <- get_stats19(2021, silent = TRUE)
        )
      )

      # update data download
      if (shiny::isRunning()) {progress$inc(3/n_data)}

      # 3 Read in main dashboard data -----------------------------------------#

      # read in stats19 2022 data
      suppressMessages(
        suppressWarnings(
          stats19_2022 <- get_stats19(2022, silent = TRUE)
        )
      )

      # update data download
      if (shiny::isRunning()) {progress$inc(4/n_data)}

      # combine data
      self$stats19 <- rbind(
        stats19_2021,
        stats19_2022
        ) |>
        mutate(number_of_casualties = as.numeric(number_of_casualties),
               number_of_vehicles = as.numeric(number_of_vehicles))

      # update data download
      if (shiny::isRunning()) {progress$inc(5/n_data)}

      # log - make it red as should only happen once
      cat_where(where = paste0(whereami(), " - created stats19"), color = "red")


    },

    #' @description
    #' Filter dates from Date Range & Period UI input.
    #'
    #' Note: currently filters using a reference data frame, needs testing against
    #' approach that does it on the fly.
    #'
    #' @param period period input from UI
    #' @param start start date
    #' @param end end date

    filter_ref_date = function(period, start, end) {

      # if date_period is not set, filter by the date range
      if (period == "all" | is.null(period)) {

        output <- self$date_ref |>
          filter(date >= start & date <= end)

      } else {

        # work out number of periods
        n_periods <- as.integer(gsub("_.*", "", period))

        # if current, add the current period (i.e. 0 in the rolling fields)
        if (grepl("current", period)) {
          n_periods <- 0:n_periods
        } else {
          n_periods <- 1:n_periods
        }

        # filter data
        if (grepl("week", period)) {

          output <- self$date_ref |>
            filter(roll_week %in% n_periods)

        } else if (grepl("month", period)) {

          output <- self$date_ref |>
            filter(roll_month %in% n_periods)

        } else if (grepl("f_quarter", period)) {

          output <- self$date_ref |>
            filter(roll_f_quarter %in% n_periods)

        } else if (grepl("f_year", period)) {

          output <- self$date_ref |>
            filter(roll_f_year %in% n_periods)

        } else if (grepl("year", period)) {

          output <- self$date_ref |>
            filter(roll_year %in% n_periods)

        } else {

          warning("looks like date filter not applied...")
        }

      }

      return(output)
    },


    #' @description
    #' Calculate dashboard metrics
    #'
    #' Function gets metric data. Either calculated within function, or calls
    #' another function where more complex.
    #'
    #' @param metric_ids ID of the metric to be used
    #' @param ... other fields to group data by
    #'
    #' @importFrom PHEindicatormethods phe_rate
    #' @importFrom janitor clean_names
    #' @importFrom stats setNames

    app_metrics = function(metric_ids,
                            ...) {


      #metric_ids <- input$metric_id
      #r6_data <- dash_data
      #vars <- c("month", "speed_limit", "calender_year")

      # get metric details
      metric_details <- self$metric_meta |>
        filter(metric_id %in% metric_ids)

      # check metric id in r6 meta
      if (nrow(metric_details) == 0) {
        stop("metric_ids wrong")
      }

      # get the data
      data <- self$stats19

      # if using date fields in the ..., add them in from date_ref (again, needs
      # assessing if more efficient on the fly than storing date_ref)
      dot_fields <- list(...)

      date_dot_fields <- dot_fields[dot_fields %in% colnames(self$date_ref) &
                                      dot_fields != "date"]

      if (length(date_dot_fields) > 0) {

        date_fields <- c(as.character(date_dot_fields), "date")

        data <- data |>
          left_join(self$date_ref |>
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
            filter(date >= self$date_range[1] & date <= self$date_range[2]) |>
            mutate(metric_id = m_id) |>
            count(..., name = "value") |>
            mutate(metric_id = m_id,
                   lowercl = NA,
                   uppercl = NA,
                   numerator = NA,
                   denominator = value)

          output_data <- rbind(output_data, metric_data)

          # metric 2: Rate of casualties per collision ------------------------------#

        } else if (m_id == "rate_casual_per_collision") {

          # data
          metric_data <- data |>
            filter(date >= self$date_range[1] & date <= self$date_range[2]) |>
            mutate(metric_id = m_id) |>
            group_by(...) |>
            summarise(denominator = n(),
                      numerator = sum(number_of_casualties),
                      .groups = "drop") |>
            phe_rate(x = numerator,
                     n = denominator,
                     multiplier = 100) |>
            mutate(metric_id = m_id) |>
            select(-confidence, -statistic, -method)

          output_data <- rbind(output_data, metric_data)

        }
      }

      output_data <- janitor::clean_names(output_data) |>
        remove_field(na)

      # return data
      return(setNames(list(output_data, metric_details),
                      c("data", "details")))
    }
  )
)


