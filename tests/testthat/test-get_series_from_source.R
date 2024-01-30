
test_that("get_series_from_source works", {
  expect_equal( {
    df <- get_series_from_source()
    is.data.frame(df )
  },  T )
})