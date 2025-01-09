
# Test utils_server functions
#------------------------------------------------------------------------------#

# create dash_data object
dash_data <- app_data$new()




# test remove_field ------------------------------------------------------------

testthat::test_that("utils_server - remove_field", {

  df <- data.frame(a = 1, b = 2)

  test_1 <- remove_field(df, a)

  expect_equal(colnames(df), c("a", "b"))
  expect_equal(colnames(test_1), "b")
  expect_no_error(remove_field(df, a))
  expect_no_error(remove_field(df, z))



})


# test filter_ref_date ---------------------------------------------------------

testthat::test_that("utils_server - filter_ref_date", {

  # set period options (add these to R6 and read in to here & UI from there?)
  available_periods <- c(
    "All" = "all",
    "Last week" = "1_week",
    "Last 4 weeks" = "4_week",
    "Last Month" = "1_month",
    "Last 3 months" = "3_month",
    "Last 4 months" = "4_month",
    "Last 6 months" = "6_month",
    "Last Year" = "1_year",
    "Last 18 months" = "18_month",
    "Last 2 years" = "2_year",
    "Last 3 years" = "3_year"
    )

  # date range filter looks correct -------------------------------------------#

  end_date <- as.Date("2022-11-30")
  start_date <- as.Date("2021-02-01")

  date_1 <- filter_ref_date(df = dash_data$date_ref,
                            start = start_date,
                            end = end_date,
                            period = available_periods["All"])

  expect_true(max(date_1$date) == end_date)
  expect_true(min(date_1$date) == start_date)


  end_date <- as.Date("2022-01-30")
  start_date <- as.Date("2022-01-01")

  date_2 <- filter_ref_date(df = dash_data$date_ref,
                            start = start_date,
                            end = end_date,
                            period = available_periods["All"])

  expect_true(max(date_2$date) == end_date)
  expect_true(min(date_2$date) == start_date)

  # period looks correct ------------------------------------------------------#

  # all periods - should default to using date range
  all_periods <- filter_ref_date(df = dash_data$date_ref,
                                 start = start_date,
                                 end = end_date,
                                 period = available_periods["All"])

  expect_equal(date_2, all_periods)

  # Last week - should be the last complete week in the data
  last_week <- filter_ref_date(df = dash_data$date_ref,
                               start = start_date,
                               end = end_date,
                               period = available_periods["Last week"])

  expect_true(nrow(last_week) == 7)
  expect_true(weekdays(last_week$date[last_week$date == min(last_week$date)]) == "Monday")
  expect_true(weekdays(last_week$date[last_week$date == max(last_week$date)]) == "Sunday")

  # the proceeding week should have less than 7 days (i.e. the next week should
  # not be a complete week)
  n_days_next_week <- dash_data$date_ref |>
    filter(date > max(last_week$date)) |>
    nrow()

  expect_equal(n_days_next_week, 6)

  # last 4 weeks
  last_4_weeks <- filter_ref_date(df = dash_data$date_ref,
                                  start = start_date,
                                  end = end_date,
                                  period = available_periods["Last 4 weeks"])

  expect_true(nrow(last_4_weeks) == 28)
  expect_true(weekdays(last_4_weeks$date[last_4_weeks$date == min(last_4_weeks$date)]) == "Monday")
  expect_true(weekdays(last_4_weeks$date[last_4_weeks$date == max(last_4_weeks$date)]) == "Sunday")

  # the proceeding week should have less than 7 days (i.e. the next week should
  # not be a complete week)
  n_days_next_week <- dash_data$date_ref |>
    filter(date > max(last_4_weeks$date)) |>
    nrow()

  expect_equal(n_days_next_week, 6)

  # last month
  last_4_weeks <- filter_ref_date(df = dash_data$date_ref,
                                  start = start_date,
                                  end = end_date,
                                  period = available_periods["Last 4 weeks"])

  expect_true(nrow(last_4_weeks) == 28)
  expect_true(weekdays(last_4_weeks$date[last_4_weeks$date == min(last_4_weeks$date)]) == "Monday")
  expect_true(weekdays(last_4_weeks$date[last_4_weeks$date == max(last_4_weeks$date)]) == "Sunday")

  # the proceeding week should have less than 7 days (i.e. the next week should
  # not be a complete week)
  n_days_next_week <- dash_data$date_ref |>
    filter(date > max(last_4_weeks$date)) |>
    nrow()

  expect_equal(n_days_next_week, 6)






})
