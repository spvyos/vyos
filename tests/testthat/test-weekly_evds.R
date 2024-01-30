test_that("weekly evds  works", {
  expect_equal(
    {
      template <- "
TP.AB.N01#
TP.AB.N02#
TP.PR.ARZ13 # Para arzi
bie_KYBKATFON
"
      start_date <- "2020/01/01"
      # TP.PR.ARZ13 # Para arzi
      obj <- get_series(template,
        start_date = start_date,
        cache = F,
        freq = "week",
        debug = F
      )
      df <- obj$data
      df <- df[-c(seq(nrow(df) - 5)), ]
      df

      is.data.frame(df)
    },
    T
  )
})
