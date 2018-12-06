context("Tests for the package")

test_that("Simple test collection", {
  testthat::expect_output(sample_function(), "Hello world")
})

