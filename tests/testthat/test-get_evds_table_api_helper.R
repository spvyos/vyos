test_that("get_evds_table_api_helper works", {
  expect_equal(
    {
      df <- get_evds_table_api_helper("bie_altingr", F)
      is.data.frame(df)
    },
    T
  )
})
