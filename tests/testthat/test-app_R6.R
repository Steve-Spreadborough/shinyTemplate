

# Test app_r6  app_data class methods
#------------------------------------------------------------------------------#

# create dash_data object
dash_data <- app_data$new()

dash_data$date_range <- dash_data$data_date_range

# test metric_meta ------------------------------------------------------------#

testthat::test_that("app_data - metric_meta", {

  expect_equal(
    sort(colnames(dash_data$metric_meta)),
    sort(c("metric_id", "metric_name", "metric_detail", "value_type"))
  )


})

# test filter_ref_date --------------------------------------------------------#

testthat::test_that("app_data - filter_ref_date", {

  # date range filter looks correct -------------------------------------------#

  end_date <- as.Date("2022-11-30")
  start_date <- as.Date("2021-02-01")

  date_1 <- dash_data$filter_ref_date(
                            start = start_date,
                            end = end_date,
                            period = "date_range"
                            )

  expect_true(max(date_1$date) == end_date)
  expect_true(min(date_1$date) == start_date)


  end_date <- as.Date("2022-01-30")
  start_date <- as.Date("2022-01-01")

  date_2 <- dash_data$filter_ref_date(
    start = start_date,
    end = end_date,
    period = "date_range"
    )

  expect_true(max(date_2$date) == end_date)
  expect_true(min(date_2$date) == start_date)

  # period looks correct ------------------------------------------------------#

  # all periods - should default to using date range
  all_periods <- dash_data$filter_ref_date(
    start = start_date,
    end = end_date,
    period = "date_range"
    )

  expect_equal(date_2, all_periods)

  # current week
  current_week <- dash_data$filter_ref_date(
    start = start_date,
    end = end_date,
    period = "0_week_current"
  )

  expect_true(max(current_week$date) == max(as.Date(dash_data$stats19$date)))
  expect_true(weekdays(min(current_week$date)) == "Monday")

  # current month
  current_month <- dash_data$filter_ref_date(
    start = start_date,
    end = end_date,
    period = "0_month_current"
  )

  expect_true(max(current_month$date) == max(as.Date(dash_data$stats19$date)))
  expect_true(day(min(current_month$date)) == 1)
  expect_true(day(max(current_month$date) + 1) == 1)

  # Current financial quarter
  current_f_quarter <- dash_data$filter_ref_date(
    start = start_date,
    end = end_date,
    period = "0_f_quarter_current"
  )

  expect_true(max(current_f_quarter$date) == max(as.Date(dash_data$stats19$date)))
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

  expect_true(max(current_12_months$date) == max(as.Date(dash_data$stats19$date)))
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

  expect_true(max(current_f_year$date) == max(as.Date(dash_data$stats19$date)))
  expect_true(length(unique(current_f_year$f_year)) == 1)
  expect_true(months(min(current_f_year$date)) == "April")

  # Current 24 months
  current_24_months <- dash_data$filter_ref_date(
    start = start_date,
    end = end_date,
    period = "23_month_current"
  )

  expect_true(max(current_24_months$date) == max(as.Date(dash_data$stats19$date)))
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

  # last financial quarter
  last_f_quarter<- dash_data$filter_ref_date(
    start = start_date,
    end = end_date,
    period = "1_f_quarter"
  )

  expect_true((max(last_f_quarter$date) + 1) == min(current_f_quarter$date))

  # Last 12 months
  last_12_months <- dash_data$filter_ref_date(
    start = start_date,
    end = end_date,
    period = "12_month"
  )

  expect_false(max(last_12_months$date) == max(as.Date(dash_data$stats19$date)))
  expect_true(length(unique(last_12_months$month_year)) == 12)
  expect_true(length(unique(last_12_months$month)) == 12)
  expect_true(last_12_months |>
                select(month, calender_year) |>
                unique() |>
                nrow() == 12)

  # last 24 months
  last_24_months <- dash_data$filter_ref_date(
    start = start_date,
    end = end_date,
    period = "23_month"
  )

  expect_false(max(last_24_months$date) == max(as.Date(dash_data$stats19$date)))
  expect_true(length(unique(last_24_months$month)) == 12)
})

# test app_metrics ------------------------------------------------------------#

testthat::test_that("app_data - app_metrics", {

  # note: test variation of grouping variables from explore plot options,
  # avoid selecting at random as need meaningful comparison on how long they
  # take with shiny.benchmark

  mets <- dash_data$metric_meta

  # metric test 1 - first metric by date --------------------------------------#

  x_axis <- "date"

  test_1 <- dash_data$app_metrics(
    metric_ids = mets$metric_id[1],
    {{ sym(x_axis) }}
    )

  expect_true(
    test_1$details$metric_id == mets$metric_id[1]
    )

  expect_true(
    test_1$details$metric_name ==
      mets$metric_name[mets$metric_id == mets$metric_id[1]]
  )

  expect_true(
    x_axis %in% colnames(test_1$data)
  )

  expect_true(
    max(test_1$data$date) == max(dash_data$stats19$date)
  )

  # metric test 2 - first metric by week start & grouping ---------------------#

  x_axis <- "week_start"
  group <- dash_data$explore_group_facet[3]

  test_2 <- dash_data$app_metrics(
    metric_ids = mets$metric_id[1],
    {{ sym(x_axis) }},
    {{ sym(group) }}
  )

  expect_true(
    test_2$details$metric_id == mets$metric_id[1]
  )

  expect_true(
    test_2$details$metric_name ==
      mets$metric_name[mets$metric_id == mets$metric_id[1]]
  )

  expect_true(
    x_axis %in% colnames(test_2$data)
  )

  expect_true(
    group %in% colnames(test_2$data)
  )

  # metric test 3 - first metric by rolling 3 months & group & facet ----------#
  x_axis <- "roll_3month"
  group <- dash_data$explore_group_facet[3]
  facet <- dash_data$explore_group_facet[4]

  test_3 <- dash_data$app_metrics(
    metric_ids = mets$metric_id[1],
    {{ sym(x_axis) }},
    {{ sym(group) }},
    {{ sym(facet) }}
  )

  expect_true(
    x_axis %in% colnames(test_3$data)
  )

  expect_true(
    group %in% colnames(test_3$data)
  )

  expect_true(
    facet %in% colnames(test_3$data)
  )

  # metric test 4 - first metric by rolling 4 weeks & group & facet -----------#
  x_axis <- "roll_4week"
  group <- dash_data$explore_group_facet[4]
  facet <- dash_data$explore_group_facet[5]

  test_4 <- dash_data$app_metrics(
    metric_ids = mets$metric_id[1],
    {{ sym(x_axis) }},
    {{ sym(group) }},
    {{ sym(facet) }}
  )

  expect_true(
    x_axis %in% colnames(test_4$data)
  )

  expect_true(
    group %in% colnames(test_4$data)
  )

  expect_true(
    facet %in% colnames(test_4$data)
  )

  # metric test 5 - 2 metrics, group by metric & another facet ----------------#
  x_axis <- "roll_4week"
  facet <- dash_data$explore_group_facet[5]

  test_5 <- dash_data$app_metrics(
    metric_ids = mets$metric_id[1:2],
    {{ sym(x_axis) }},
    {{ sym(facet) }}
  )

  expect_equal(
    sort(unique(test_5$data$metric_id)),
    sort(mets$metric_id[1:2])
  )

  expect_true(
    x_axis %in% colnames(test_5$data)
  )

  expect_true(
    facet %in% colnames(test_5$data)
  )

  # metric 6 - first metric, edited dates -------------------------------------#
  test_date_range <- c(dash_data$data_date_range[1] + 100,
                       dash_data$data_date_range[2] - 100)

  dash_data$date_range <- test_date_range

  x_axis <- "date"

  test_6 <- dash_data$app_metrics(
    metric_ids = mets$metric_id[1],
    {{ sym(x_axis) }}
  )

  expect_true(
    x_axis %in% colnames(test_6$data)
  )

  expect_true(
    min(test_6$data$date) == as.Date(test_date_range[1]) # assumes all dates in data
  )

  expect_true(
    max(test_6$data$date) == test_date_range[2] # assumes all dates in data
  )


  # metric 7 - 2 metrics, edited dates, grouping variables --------------------#
  test_date_range <- c(dash_data$data_date_range[1] + 50,
                       dash_data$data_date_range[2] - 50)

  dash_data$date_range <- test_date_range

  x_axis <- "date"
  facet <- dash_data$explore_group_facet[5]
  group <- "calender_year"

  test_7 <- dash_data$app_metrics(
    metric_ids = mets$metric_id[1:2],
    {{ sym(x_axis) }},
    {{ sym(facet) }},
    {{ sym(group) }}
  )

  expect_true(
    x_axis %in% colnames(test_7$data)
  )

  expect_true(
    min(test_7$data$date) == as.Date(test_date_range[1]) # assumes all dates in data
  )

  expect_true(
    max(test_7$data$date) == test_date_range[2] # assumes all dates in data
  )

})
