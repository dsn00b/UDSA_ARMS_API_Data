context("pull data")

test_that("erroneous report name", {
  
  expect_error( pull_data(c(2000,"2001"),report="Farm Business Balance Sheet") )
  
})


