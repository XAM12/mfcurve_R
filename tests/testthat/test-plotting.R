test_that("mfcurve_plotting generates a valid plotly object", {
  # Example data
  stats <- data.frame(
    group = c("A_B", "A_C", "B_C"),
    mean_outcome = c(10, 15, 20),
    sd_outcome = c(2, 3, 4),
    n = c(20, 25, 30),
    ci_lower = c(9, 14, 19),
    ci_upper = c(11, 16, 21),
    rank = c(1, 2, 3)
  )
  factors <- c("factor1", "factor2")

  # Generate plot
  plot <- mfcurve_plotting(stats, factors, outcome = "wage")

  # Check that the plot is a plotly object
  expect_s3_class(plot, "plotly")
})

test_that("mfcurve_plotting fails with missing required columns", {
  # Example data with missing columns
  stats_incomplete <- data.frame(
    group = c("A_B", "A_C"),
    mean_outcome = c(10, 15)
  )
  factors <- c("factor1", "factor2")

  # Expect an error
  expect_error(
    mfcurve_plotting(stats_incomplete, factors, outcome = "wage"),
    "The input data must include the columns: rank, mean_outcome, ci_lower, ci_upper, and group."
  )
})

test_that("mfcurve_plotting generates plots with and without a title", {
  # Example data
  stats <- data.frame(
    group = c("A_B", "A_C", "B_C"),
    mean_outcome = c(10, 15, 20),
    sd_outcome = c(2, 3, 4),
    n = c(20, 25, 30),
    ci_lower = c(9, 14, 19),
    ci_upper = c(11, 16, 21),
    rank = c(1, 2, 3)
  )
  factors <- c("factor1", "factor2")

  # Generate plot with title
  plot_with_title <- mfcurve_plotting(stats, factors, outcome = "wage", showTitle = TRUE)
  expect_s3_class(plot_with_title, "plotly")

  # Generate plot without title
  plot_without_title <- mfcurve_plotting(stats, factors, outcome = "wage", showTitle = FALSE)
  expect_s3_class(plot_without_title, "plotly")
})

test_that("mfcurve_plotting handles different factor levels correctly", {
  # Example data
  stats <- data.frame(
    group = c("A_B", "A_C", "B_C", "C_D"),
    mean_outcome = c(10, 15, 20, 25),
    sd_outcome = c(2, 3, 4, 5),
    n = c(20, 25, 30, 35),
    ci_lower = c(9, 14, 19, 24),
    ci_upper = c(11, 16, 21, 26),
    rank = c(1, 2, 3, 4)
  )
  factors <- c("factor1", "factor2")

  # Generate plot
  plot <- mfcurve_plotting(stats, factors, outcome = "wage")

  # Check that the plot is a plotly object
  expect_s3_class(plot, "plotly")
})
