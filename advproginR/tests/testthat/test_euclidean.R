library(advproginR)

context("Euclidean algorithm")

test_that("euclidean only accepts numeric scalar input",{
  expect_error(euclidean("abc",5))
  expect_error(euclidean(c(1,2,3),8))
  expect_error(euclidean(0,12))
  
})

test_that("euclidean produces correct output",{
  expect_equal(euclidean(2401,343),7^3)
  expect_equal(class(euclidean(20,8)),"integer")
  
})