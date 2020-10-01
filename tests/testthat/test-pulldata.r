context("pull data")

test_that("erroneous report name throws error", {
  
  expect_error( pull_data(c(2000,"2001"),report="blabla") )
  
})


test_that("correct report name and year input does not throw error", {
  
  expect_error( pull_data(c(2000,"2001"),report="Farm Business Balance Sheet"), NA )
  
})

test_that("incorrect input for year throws error", {
  
  expect_error( pull_data(c(2000,"2001k"),report="Farm Business Balance Sheet") )
  
})

test_that("correct input for year does not throw error", {
  
  expect_error( pull_data(c(2000,"2001"),report="Farm Business Balance Sheet"), NA )
  
})


test_that("incorrect input for state throws error", {
  
  expect_error( pull_data(c(2000,"2001"),report="Farm Business Balance Sheet", state="SE") )
  
})


test_that("state='all' does not throw error", {
  
  expect_error( pull_data(c(2000,"2001"),report="Farm Business Balance Sheet", state="all"), NA )
  
})

test_that("state='all' does not throw error", {
  
  expect_error( pull_data(c(2000,"2001"),report="Farm Business Balance Sheet", state="all"), NA )
  
})



