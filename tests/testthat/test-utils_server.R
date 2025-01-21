
# Test utils_server functions
#------------------------------------------------------------------------------#

# create dash_data object
dash_data <- app_data$new()

dash_data$date_range <- dash_data$data_date_range

# test remove_field -----------------------------------------------------------#

testthat::test_that("utils_server - remove_field", {

  df <- data.frame(a = 1, b = 2)

  test_1 <- remove_field(df, a)

  expect_equal(colnames(df), c("a", "b"))
  expect_equal(colnames(test_1), "b")
  expect_no_error(remove_field(df, a))
  expect_no_error(remove_field(df, z))



})

# explore_plot_details --------------------------------------------------------#

testthat::test_that("utils_server - explore_plot_details", {

  mets <- dash_data$metric_meta



  # metric test 1 - first metric by date --------------------------------------#

  test_input <- list()

  test_input$metric_id <- dash_data$metric_meta$metric_id[1]
  test_input$plot_group <- dash_data$explore_group_facet[1]
  test_input$plot_facet <- dash_data$explore_group_facet[1]
  test_input$plot_x_axis <- dash_data$explore_x_axis[1]
  test_input$plot_y_axis <-  dash_data$explore_y_axis[1]

  test_1 <- explore_plot_details(
    ui_inputs = test_input,
    mod_data = dash_data
  )

  expect_true(
    test_1$details$metric_id == mets$metric_id[1]
  )

  expect_true(
    test_1$details$metric_name ==
      mets$metric_name[mets$metric_id == mets$metric_id[1]]
  )

  expect_true(
    test_input$plot_x_axis %in% colnames(test_1$data)
  )

  expect_true(
    max(test_1$data$date) == max(dash_data$stats19$date)
  )

  expect_equal(
    sort(names(test_1)),
    sort(c("data", "details", "plot_x_axis", "plot_y_axis", "x_name",
           "y_name", "plot_group", "plot_facet", "title","subtitle"))
  )

  # metric test 2 - first metric by week start & grouping ---------------------#

  test_input <- list()

  test_input$metric_id <- dash_data$metric_meta$metric_id[1]
  test_input$plot_group <- dash_data$explore_group_facet[3]
  test_input$plot_facet <- dash_data$explore_group_facet[1]
  test_input$plot_x_axis <- "week_start"
  test_input$plot_y_axis <-  dash_data$explore_y_axis[1]

  test_2 <- explore_plot_details(
    ui_inputs = test_input,
    mod_data = dash_data
  )

  expect_true(
    test_2$details$metric_id == mets$metric_id[1]
  )

  expect_true(
    test_2$details$metric_name ==
      mets$metric_name[mets$metric_id == mets$metric_id[1]]
  )

  expect_true(
    test_input$plot_x_axis %in% colnames(test_2$data)
  )

  expect_true(
    test_input$plot_group %in% colnames(test_2$data)
  )

  # metric test 3 - first metric by rolling 3 months & group & facet ----------#

  test_input <- list()

  test_input$metric_id <- dash_data$metric_meta$metric_id[1]
  test_input$plot_group <- dash_data$explore_group_facet[3]
  test_input$plot_facet <- dash_data$explore_group_facet[4]
  test_input$plot_x_axis <- "roll_3month"
  test_input$plot_y_axis <-  dash_data$explore_y_axis[1]

  test_3 <- explore_plot_details(
    ui_inputs = test_input,
    mod_data = dash_data
  )

  expect_true(
    test_input$plot_x_axis  %in% colnames(test_3$data)
  )

  expect_true(
    test_input$plot_group %in% colnames(test_3$data)
  )

  expect_true(
    test_input$plot_facet %in% colnames(test_3$data)
  )

  # metric test 4 - first metric by rolling 4 weeks & group & facet -----------#

  test_input <- list()

  test_input$metric_id <- dash_data$metric_meta$metric_id[1]
  test_input$plot_group <- dash_data$explore_group_facet[4]
  test_input$plot_facet <- dash_data$explore_group_facet[5]
  test_input$plot_x_axis <- "roll_4week"
  test_input$plot_y_axis <-  dash_data$explore_y_axis[1]

  test_4 <- explore_plot_details(
    ui_inputs = test_input,
    mod_data = dash_data
  )

  expect_true(
    test_input$plot_x_axis %in% colnames(test_4$data)
  )

  expect_true(
    test_input$plot_group %in% colnames(test_4$data)
  )

  expect_true(
    test_input$plot_facet %in% colnames(test_4$data)
  )



  # metric test 5 - 2 metrics, group by metric & another facet ----------------#

  test_input <- list()

  test_input$metric_id <- dash_data$metric_meta$metric_id[1:2]
  test_input$plot_group <- dash_data$explore_group_facet[1]
  test_input$plot_facet <- dash_data$explore_group_facet[5]
  test_input$plot_x_axis <- "roll_4week"
  test_input$plot_y_axis <-  dash_data$explore_y_axis[1]

  test_5 <- explore_plot_details(
    ui_inputs = test_input,
    mod_data = dash_data
  )

  expect_equal(
    sort(unique(test_5$data$metric_id)),
    sort(mets$metric_id[1:2])
  )

  expect_true(
    test_input$plot_x_axis %in% colnames(test_5$data)
  )

  expect_true(
    test_input$plot_facet %in% colnames(test_5$data)
  )

  # metric 6 - first metric, edited dates -------------------------------------#

  test_date_range <- c(dash_data$data_date_range[1] + 100,
                       dash_data$data_date_range[2] - 100)

  dash_data$date_range <- test_date_range

  test_input <- list()

  test_input$metric_id <- dash_data$metric_meta$metric_id[1]
  test_input$plot_group <- dash_data$explore_group_facet[1]
  test_input$plot_facet <- dash_data$explore_group_facet[1]
  test_input$plot_x_axis <- "date"
  test_input$plot_y_axis <-  dash_data$explore_y_axis[1]

  test_6 <- explore_plot_details(
    ui_inputs = test_input,
    mod_data = dash_data
  )

  expect_true(
    test_input$plot_x_axis %in% colnames(test_6$data)
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


  test_input <- list()

  test_input$metric_id <- dash_data$metric_meta$metric_id[1]
  test_input$plot_group <- "calender_year"
  test_input$plot_facet <- dash_data$explore_group_facet[5]
  test_input$plot_x_axis <- "date"
  test_input$plot_y_axis <-  dash_data$explore_y_axis[1]

  test_7 <- explore_plot_details(
    ui_inputs = test_input,
    mod_data = dash_data
  )

  # note that it should have changed test_input$plot_x_axis from 'date' to
  # 'day_month' as grouping the data by year
  expect_true(
    "day_month" %in% colnames(test_7$data)
  )

  # x axis date but grouping by year, should have changed 'date' to 'day_month'
  expect_false(
    test_input$plot_x_axis %in% colnames(test_7$data)
  )

  expect_true(
    "day_month" %in% colnames(test_7$data)
  )

})
