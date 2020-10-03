context("pull_data")

test_that("erroneous report name throws error", {
  
  expect_error( pull_data(c(2000,"2001"),report="blabla") )
  
})


test_that("correct report name and year input does not throw error", {
  
  expect_error( pull_data(c(2000,"2001"),
                          report="Farm Business Balance Sheet"), NA )
  
})

test_that("incorrect input for year throws error", {
  
  expect_error( pull_data(c(2000,"2001k"),
                          report="Farm Business Balance Sheet") )
  
})

test_that("incorrect input for state throws error", {
  
  expect_error( pull_data(c(2000,"2001"),
                          report="Farm Business Balance Sheet", state="SE") )
  
})


test_that("state='all' does not throw error and returns a data.frame with 24 columns", {
  
  expect_error( pull_data(c(2000,"2001"),
                          report="Farm Business Balance Sheet", state="all"), NA )
  test_data <- pull_data(c(2000,"2001"),
                         report="Farm Business Balance Sheet", state="all")
  expect_equal(class(test_data), "data.frame")
  expect_equal(length(colnames(test_data)), 24)
  rm(test_data)
  
})