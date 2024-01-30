test_that("get_series works", {
  expect_equal(
    {
      obj <- get_series(template_test())

      all(
        is.data.frame(obj$lines$data[[1]]),
        is.data.frame(obj$lines$data[[2]]),
        is.data.frame(obj$lines$data[[3]])
      )
    },
    T
  )
})
