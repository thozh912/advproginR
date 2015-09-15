library(Lab4)

context("Linear Regression")

test_that("linreg can initialize correct fields",{
  expect_equal(linreg(Sepal.Length ~ Sepal.Width,iris)$coefficients, as.matrix(coef(lm(Sepal.Length ~ Sepal.Width,iris))),tolerance = 1e-6)
  expect_equal(class(linreg(eruptions ~ waiting,faithful))[1],"linreg")
  expect_error(linreg(34 + 43,c("ha")))
  
  
})

test_that("coefficients can get ok",{
  expect_equal(linreg(accel ~ dist,attenu)$coef(),coef(lm(accel ~ dist,attenu)))
  expect_equal(linreg(Petal.Length ~ Species,iris)$coef(),coef(lm(Petal.Length ~ Species,iris)))
})
test_that("resid are gucci",{
  expect_equal(linreg(Sepal.Length ~ Species,iris)$resid(),residuals(lm(Sepal.Length ~ Species,iris)) )
  expect_equal(linreg(mpg ~ hp,mtcars)$resid(),residuals(lm(mpg ~ hp,mtcars)))
})
test_that("pred returns correct values",{
  expect_equal(linreg(pressure ~ temperature,pressure)$pred(),predict(lm(pressure ~ temperature,pressure)))
  expect_equal(linreg(demand ~ Time,BOD)$pred(),predict(lm(demand ~ Time,BOD)))
  
})