test_that("check_if_vector works", {
  expect_equal( {
        template <- "
TP_YSSK_A1
TP_YSSK_A2
TP_YSSK_A3
TP_YSSK_A4
TP_YSSK_A5
TP_YSSK_A6
"
        correct_if_vector( template ) == template
  }, T)


    expect_equal( {
        index_vector  = c( "TP_YSSK_A1" , "TP_YSSK_A2" )

        # o <- get_series(index_vector )

        correct_if_vector( index_vector ) ==  "TP_YSSK_A1\nTP_YSSK_A2"
    }, T)



    expect_equal( {

        index_vector  = c( "TP_YSSK_A1" , "TP_YSSK_A2" )

        o <- get_series(index_vector )

        is.data.frame( o$data )

    }, T)


    expect_equal( {

        index_template   = c( "TP_YSSK_A1\nTP_YSSK_A2" )

        o <- get_series(index_template )

        is.data.frame( o$data )

    }, T)
})
