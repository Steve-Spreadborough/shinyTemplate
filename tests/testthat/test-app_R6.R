

# Test app_r6  app_data class methods
#------------------------------------------------------------------------------#

# note: have tests also


# create dash_data object
dash_data <- app_data$new()


# test filter_ref_date ---------------------------------------------------------

testthat::test_that("app_data - filter_ref_date", {

  # date range filter looks correct -------------------------------------------#

  end_date <- as.Date("2022-11-30")
  start_date <- as.Date("2021-02-01")

  date_1 <- dash_data$filter_ref_date(
                            start = start_date,
                            end = end_date,
                            period = "all"
                            )

  expect_true(max(date_1$date) == end_date)
  expect_true(min(date_1$date) == start_date)


  end_date <- as.Date("2022-01-30")
  start_date <- as.Date("2022-01-01")

  date_2 <- dash_data$filter_ref_date(
    start = start_date,
    end = end_date,
    period = "all"
    )

  expect_true(max(date_2$date) == end_date)
  expect_true(min(date_2$date) == start_date)

  # period looks correct ------------------------------------------------------#

  # all periods - should default to using date range
  all_periods <- dash_data$filter_ref_date(
    start = start_date,
    end = end_date,
    period = "all"
    )

  expect_equal(date_2, all_periods)

  dash_data$date_period_options

  # current week
  current_week <- dash_data$filter_ref_date(
    start = start_date,
    end = end_date,
    period = "0_week_current"
  )

  expect_true(max(current_week$date) == max(as.Date(dash_data$stats19$datetime)))
  expect_true(weekdays(min(current_week$date)) == "Monday")

  # current month
  current_month <- dash_data$filter_ref_date(
    start = start_date,
    end = end_date,
    period = "0_month_current"
  )

  expect_true(max(current_month$date) == max(as.Date(dash_data$stats19$datetime)))
  expect_true(day(min(current_month$date)) == 1)
  expect_true(day(max(current_month$date) + 1) == 1)

  # Current financial quarter
  current_f_quarter <- dash_data$filter_ref_date(
    start = start_date,
    end = end_date,
    period = "0_f_quarter_current"
  )

  expect_true(max(current_f_quarter$date) == max(as.Date(dash_data$stats19$datetime)))
  expect_true(length(unique(current_f_quarter$month_year)) == 3)
  expect_true(length(unique(current_f_quarter$month)) == 3)
  expect_true(length(unique(current_f_quarter$f_year)) == 1)
  expect_true(day(max(current_f_quarter$date) + 1) == 1)


  # current 12 months
  current_12_months <- dash_data$filter_ref_date(
    start = start_date,
    end = end_date,
    period = "11_month_current"
  )

  expect_true(max(current_12_months$date) == max(as.Date(dash_data$stats19$datetime)))
  expect_true(length(unique(current_12_months$month_year)) == 12)
  expect_true(length(unique(current_12_months$month)) == 12)
  expect_true(current_12_months |>
                select(month, calender_year) |>
                unique() |>
                nrow() == 12)

  # Current financial year
  current_f_year <- dash_data$filter_ref_date(
    start = start_date,
    end = end_date,
    period = "0_f_year_current"
  )

  expect_true(max(current_f_year$date) == max(as.Date(dash_data$stats19$datetime)))
  expect_true(length(unique(current_f_year$f_year)) == 1)
  expect_true(months(min(current_f_year$date)) == "April")

  # Current 24 months
  current_24_months <- dash_data$filter_ref_date(
    start = start_date,
    end = end_date,
    period = "23_month_current"
  )

  expect_true(max(current_24_months$date) == max(as.Date(dash_data$stats19$datetime)))
  expect_true(length(unique(current_24_months$month_year)) == 24)
  expect_true(length(unique(current_24_months$month)) == 12)
  expect_true(current_24_months |>
                select(month, calender_year) |>
                unique() |>
                nrow() == 24)

  # last week
  last_week <- dash_data$filter_ref_date(
    start = start_date,
    end = end_date,
    period = "1_week"
  )

  expect_true(nrow(last_week) == 7)
  expect_true(weekdays(min(last_week$date)) == "Monday")
  expect_true((max(last_week$date) + 1) == min(current_week$date))

  # last financil quarter





})
