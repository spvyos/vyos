test_that("verbose_on_off works", {
  expect_equal(
    {
      all(
        {
          options(VYOS_verbose = FALSE)
          FALSE == check_verbose_option()
        },
        {
          options(VYOS_verbose = TRUE)
          a <- check_verbose_option()
          a == TRUE
        },
        {
          options(VYOS_verbose = FALSE)
          FALSE == check_verbose_option()
        }
      )
    },
    T
  )
})
