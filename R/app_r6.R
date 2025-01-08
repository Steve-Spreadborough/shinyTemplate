

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
#'
#' @export

app_data <- R6Class(

  "app_data",
  public = list(

    #' @field date_range variable to hold start and end date from
    #' dateRangeInput. Note: this is the 'single source of truth' date range
    #' to be used across the App, and is set via dateRangeInput & updated
    #' from 'date_period' input.
    date_range = c(),

    #' @field date_period variable that holds the 'date_period' input.
    date_period = NULL,

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

      # hard coded start & end date
      start_date <- as.Date("2021-01-01")
      end_date <- as.Date("2022-12-31")

      #self$date_range[1] <- start_date
      #self$date_range[2] <- end_date

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
        mutate(
          roll_4week = as.integer(floor((roll_week+3)/4)),
          roll_3month = as.integer(floor((roll_month+2)/3)),
          roll_4month = as.integer(floor((roll_month+3)/4)),
          roll_6month = as.integer(floor((roll_month+5)/6)),
          roll_18month = as.integer(floor((roll_month+17)/18)),
          roll_year = as.integer(floor((roll_month+11)/12)),
          all_dates = 1)

      # log - make it red as should only happen once
      #cat_where(where = paste0(whereami(), " - created date_ref"), color = "red")

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

      # read in stats19 data
      suppressMessages(
        suppressWarnings(
          self$stats19 <- rbind(
            get_stats19(2021, silent = TRUE),
            get_stats19(2022, silent = TRUE)
            ) |>
            mutate(number_of_casualties = as.numeric(number_of_casualties),
                   number_of_vehicles = as.numeric(number_of_vehicles))
          )
      )

      # log - make it red as should only happen once
      #cat_where(where = paste0(whereami(), " - created stats19"), color = "red")


    }
  )
)


