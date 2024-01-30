
test_that("year_evds works", {
  expect_equal( {
      template<- "
      TP.KAMUBORCY.S9
      TP.KAMUBORCY.S10
      TP.KAMUBORCY.S11
      TP.KAMUBORCY.S12
      TP.KAMUBORCY.S13
      "
      o <- get_series( template , start_date =  lubridate::ymd( "2006/01/01" ) , freq = "year" )
      is.data.frame( o$data ) && is.data.frame(o$lines$data[[1]] )
      }, T )
})
