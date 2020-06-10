test_that("print.blblm works", {
  x <- blblm_p(mpg ~ wt * hp, data = mtcars, m = 3, B = 5000, n_cores = 4)
  z <- print.blblm(x)
  expect_type(z, "NULL")
})
