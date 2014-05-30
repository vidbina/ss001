source("helpers.R");

context("Edge detector")

test_that("detects all edges of at least a unit amplitude by default", {
  expect_that(isStep(c(0,2)), equals(T))
  expect_that(isStep(c(1,0)), equals(T))
  expect_that(isStep(c(0,0)), equals(F))
  expect_that(isStep(c(1,1)), equals(F))
  expect_that(isStep(c(0,0.99)), equals(F))
})

test_that("detects edges equal or greater to the threshold", {
  expect_that(isStep(c(0,2), thresh=0.5), equals(T))
  expect_that(isStep(c(1,0), thresh=0.5), equals(T))
  expect_that(isStep(c(0,0), thresh=0.5), equals(F))
  expect_that(isStep(c(1,1), thresh=0.5), equals(F))
  expect_that(isStep(c(0,0.99), thresh=0.5), equals(T))
})

test_that("only detects rising edges when falling is set to false", {
  expect_that(isStep(c(0,2), falling=F), equals(T))
  expect_that(isStep(c(1,0), falling=F), equals(F))
  expect_that(isStep(c(0,0), falling=F), equals(F))
  expect_that(isStep(c(1,1), falling=F), equals(F))
  expect_that(isStep(c(0,0.99), falling=F), equals(F))
})

test_that("only detects falling edges when rising is set to false", {
  expect_that(isStep(c(0,2), rising=F), equals(F))
  expect_that(isStep(c(1,0), rising=F), equals(T))
  expect_that(isStep(c(0,0), rising=F), equals(F))
  expect_that(isStep(c(1,1), rising=F), equals(F))
  expect_that(isStep(c(0,0.99), rising=F), equals(F))
})

context("Step counter")

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

context("Step indexer")

test_that("picks up the first step in a series of samples", {
  expect_that(getStepIndex(c(0,1,1,1,0)), equals(2))
  expect_that(getStepIndex(c(0,0,0,1,1)), equals(4))
  expect_that(getStepIndex(c(0,0,0,0,0)), equals(0))
  expect_that(getStepIndex(c(1,1,1,1,1)), equals(0))
  expect_that(getStepIndex(c(1,1,0,0,0)), equals(3))
  expect_that(getStepIndex(c(1,0,1,1,1)), equals(2))
})

test_that("picks up the first step from a defined point in the sample set", {
  expect_that(getStepIndex(c(0,1,1,1,0), startAt=2), equals(5))
  expect_that(getStepIndex(c(0,0,0,1,1), startAt=2), equals(4))
  expect_that(getStepIndex(c(0,0,0,0,0), startAt=2), equals(0))
  expect_that(getStepIndex(c(1,1,1,1,1), startAt=2), equals(0))
  expect_that(getStepIndex(c(1,1,0,0,0), startAt=2), equals(3))
  expect_that(getStepIndex(c(1,0,1,1,1), startAt=2), equals(3))
})

test_that("returns zero if startAt index out of bounds", {
  expect_that(getStepIndex(c(0,1,1,1,0), startAt=0), equals(0))
  expect_that(getStepIndex(c(0,0,0,1,1), startAt=0), equals(0))
  expect_that(getStepIndex(c(0,0,0,0,0), startAt=5), equals(0))
  expect_that(getStepIndex(c(1,1,1,1,1), startAt=5), equals(0))
  expect_that(getStepIndex(c(1,1,1,0,0), startAt=5), equals(0))
  expect_that(getStepIndex(c(1,0,1,1,1), startAt=5), equals(0))
})
