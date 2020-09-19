context("Testing of rxivistr check args")

test_that("Testing check args", {

  expect_error(rxivist_search(from = "2018"))

  expect_error(rxivist_search(from = "month", sortby = "downlaods"))

  expect_error(rxivist_search(category = "zology"))

})

