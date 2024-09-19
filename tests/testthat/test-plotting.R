# File: tests/testthat/test-plotting.R
library(testthat)
library(mfcurve)
library(ggplot2)

test_that("Plotting function creates a ggplot object", {
  data <- data.frame(
    group = c("A", "B"),
    mean_outcome = c(10, 20),
    ci_lower = c(6.4, 16.2),
    ci_upper = c(14.0, 24.2),
    significant = c(TRUE, TRUE)
  )

  plot <- mfcurve_plot(data)
  expect_true(inherits(plot, "ggplot"))  # Check that the plot is a ggplot object
})
