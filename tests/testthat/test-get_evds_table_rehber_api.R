
test_that("get_evds_table_info_api works", {
  expect_equal( {

    df <- get_evds_table_info_api()
    df
    "SERIE_CODE" %inn% df
  }, T )
})
