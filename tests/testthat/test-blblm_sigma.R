test_that("sigma.blblm works", {
  x <- blblm_p(mpg ~ wt * hp, data = mtcars, m = 3, B = 5000, n_cores = 4)
  z <- sigma.blblm(x, FALSE, 0.98)
  expect_type(z, type="double")
})
