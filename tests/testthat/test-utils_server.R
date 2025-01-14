
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
