test_that("blblm works", {
  x<-blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 5000)
  expect_s3_class(x, "blblm")
})
