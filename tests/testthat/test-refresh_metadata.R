context("refresh_metadata")

test_that("function works correctly", {
  
  test_meta <- refresh_metadata()
  expect_equal(class(test_meta), "list")
  expect_equal(length(test_meta), 6)
  expect_equal(names(test_meta), c("years", "states", "reports", "farmtypes", "categories", "variables"))
  rm("test_meta")
  
})