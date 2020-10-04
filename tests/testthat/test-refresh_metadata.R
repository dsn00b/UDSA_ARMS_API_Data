context("refresh_metadata")

test_that("function works correctly", {
  
  test_meta <- refresh_metadata()
  expect_equal(class(test_meta), "list")
  expect_equal(length(test_meta), 6)
  expect_equal(names(test_meta), c("years", "states", "reports", "farmtypes", "categories", "variables"))
  rm("test_meta")
  
})

### commenting below as package solves for metadata non-availability upon loading

#test_that("The object 'metadata' exists (usually because refresh-metadata was called on load)", {
  #probably only useful right after build
  
#  testfunc <- function(x){return(attributes(x))}
#  expect_error(testfunc(metadata), NA )
#})