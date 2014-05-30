context("Step counter")

source("helpers.R");

test_that("counts all edges with a minimum delta of 1 unit by default", {
  expect_that(countSteps(c(1,0,1,0,1)), equals(4))
  expect_that(countSteps(c(1,2,3,4,5)), equals(4))
  expect_that(countSteps(c(1,1,3,3,2)), equals(2))
  expect_that(countSteps(c(1,1,1,1,1)), equals(0))
  expect_that(countSteps(c(1,1,0.5,0.5,0)), equals(0))
})

test_that("counts only rising edges when falling is false", {
  expect_that(countSteps(c(1,0,1,0,1), falling=F), equals(2))
  expect_that(countSteps(c(1,2,3,4,5), falling=F), equals(4))
  expect_that(countSteps(c(8,6,4,2,0), falling=F), equals(0))
  expect_that(countSteps(c(1,2,3,3,2), falling=F), equals(2))
  expect_that(countSteps(c(1,1,1,1,1), falling=F), equals(0))
  expect_that(countSteps(c(1,1,0.5,0.5,0), falling=F), equals(0))
})

test_that("counts only falling edges when rising is false", {
  expect_that(countSteps(c(1,0,1,0,1), rising=F), equals(2))
  expect_that(countSteps(c(1,2,3,4,5), rising=F), equals(0))
  expect_that(countSteps(c(8,6,4,2,0), rising=F), equals(4))
  expect_that(countSteps(c(1,2,3,3,2), rising=F), equals(1))
  expect_that(countSteps(c(1,1,1,1,1), rising=F), equals(0))
  expect_that(countSteps(c(1,1,0.5,0.5,0), rising=F), equals(0))
})

test_that("has customizable threshold", {
  expect_that(countSteps(c(1,1,0.5,0.5,0), thresh=0.5), equals(2))
  expect_that(countSteps(c(1,0.5,0.2,0.7), thresh=0.5), equals(2))
  expect_that(countSteps(c(1,0.5,0.2,0.7,0.1), thresh=0.5), equals(3))
  expect_that(countSteps(c(1,0.5,0.2,0.7,0.1), thresh=0.5, rising=F), equals(2))
  expect_that(countSteps(c(1,0.5,0.2,0.7,0.1), thresh=0.5, falling=F), equals(1))
  expect_that(countSteps(c(1,0.5,0.2,0.8), thresh=0.6), equals(1))
  expect_that(countSteps(c(1,0.5,0.2,0.8), thresh=1), equals(0))
})
