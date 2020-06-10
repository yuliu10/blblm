test_that("blblm_p works", {
  x<-blblm_p(mpg ~ wt * hp, data = mtcars, m = 10, B = 5000, n_cores = 4)
  expect_s3_class(x, "blblm")
})

