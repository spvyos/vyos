
test_that("quick_check_evds - fails successfuly", {
 expect_equal({
   quick_check_evds("INVALID_KEY_WILL_FAIL" )} ,  F  )
})
test_that("quick_check_evds +  works", {
  expect_equal( {
   check =  quick_check_evds( get_api_key("evds" ) )
check
  }, T  )
})