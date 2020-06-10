test_that("confint.blblm works", {
  x <- blblm_p(mpg ~ wt * hp, data = mtcars, m = 3, B = 5000, n_cores = 4)
  z <- confint.blblm(x,c("wt", "hp"))
  expect_s3_class(x, "blblm")
  expect_type(z, type = "double")
})
