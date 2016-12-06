
library(testthat)
source("./../../R/standard_deviation.R", local = TRUE)

context("Testing standard deviation and error")

test_that('standard deviation functions and is unbiased', {
  expect_equal(standard_deviation(c(0, 1)), 0.7071068, tolerance = 1e-6)
  expect_equal(standard_deviation(c(0, 1, 2)), 1)
  expect_equal(standard_deviation(c(-1, 0, 1)), 1)
  expect_equal(standard_deviation(c(100, 100, 100)), 0)
})

test_that('erroronous standard deviation entries', {
  expect_error(standard_deviation(c(0)))
  expect_error(standard_deviation(0))
  expect_error(standard_deviation())
  expect_error(standard_deviation(c("ZERO", "ONE")))
})

test_that('standard error functions and is unbiased', {
  expect_equal(standard_error(c(0, 1)), 0.5)
  expect_equal(standard_error(c(0, 1, 2)), 0.577, tolerance = 1e-3)
  expect_equal(standard_error(c(-1, 0, 1)), 0.577, tolerance = 1e-3)
  expect_equal(standard_error(c(100, 100, 100)), 0)
})

test_that('erroronous standard error entries', {
  expect_error(standard_error(c(0)))
  expect_error(standard_error(0))
  expect_error(standard_error())
  expect_error(standard_error(c("ZERO", "ONE")))
})
