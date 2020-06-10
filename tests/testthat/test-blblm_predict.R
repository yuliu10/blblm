test_that("predict.blblm works", {
  x <- blblm_p(mpg ~ wt * hp, data = mtcars, m = 3, B = 5000, n_cores = 4)
  z <- predict.blblm(x, mtcars, FALSE, 0.95)
  expect_type(z, "double")
})
