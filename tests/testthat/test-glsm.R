test_that("glsm runs without error", {
  data(mtcars)
  mtcars$cyl <- factor(mtcars$cyl)
  modelo <- glsm(cyl ~ mpg + wt, data = mtcars)
  expect_s3_class(modelo, "glsm")
})

test_that("predict works", {
  data(mtcars)
  mtcars$cyl <- factor(mtcars$cyl)
  modelo <- glsm(cyl ~ mpg + wt, data = mtcars)
  preds <- predict(modelo, type = "response")
  expect_true(is.matrix(preds$predictions))
})
