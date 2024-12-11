test_that("mfcurve_stat_testing performs mean-based tests correctly", {
  data <- data.frame(
    group = c("A", "B", "C"),
    mean_outcome = c(15, 20, 25),
    sd_outcome = c(5, 4, 6),
    n = c(10, 12, 15)
  )
  result <- mfcurve_stat_testing(data, test = "mean", alpha = 0.05)

  # Recalculate expected t-values and significance
  expected_t_values <- c(
    (15 - mean(data$mean_outcome)) / (5 / sqrt(10)),
    (20 - mean(data$mean_outcome)) / (4 / sqrt(12)),
    (25 - mean(data$mean_outcome)) / (6 / sqrt(15))
  )
  expected_p_values <- 2 * pt(-abs(expected_t_values), df = data$n - 1)
  expected_significant <- expected_p_values < 0.05

  expect_equal(result$t_value, expected_t_values, tolerance = 1e-6)
  expect_equal(result$significant, expected_significant)
})

test_that("mfcurve_stat_testing performs zero-based tests correctly", {
  data <- data.frame(
    group = c("A", "B", "C"),
    mean_outcome = c(5, -3, 10),
    sd_outcome = c(2, 1.5, 3),
    n = c(10, 12, 15)
  )
  result <- mfcurve_stat_testing(data, test = "zero", alpha = 0.05)

  # Recalculate expected t-values and significance
  expected_t_values <- c(
    5 / (2 / sqrt(10)),
    -3 / (1.5 / sqrt(12)),
    10 / (3 / sqrt(15))
  )
  expected_p_values <- 2 * pt(-abs(expected_t_values), df = data$n - 1)
  expected_significant <- expected_p_values < 0.05

  expect_equal(result$t_value, expected_t_values, tolerance = 1e-6)
  expect_equal(result$significant, expected_significant)
})

test_that("mfcurve_stat_testing handles invalid inputs correctly", {
  data <- data.frame(
    group = c("A", "B", NA),
    mean_outcome = c(15, NA, 25),
    sd_outcome = c(5, 4, NA),
    n = c(10, 12, 15)
  )

  expect_error(mfcurve_stat_testing(data, test = "mean", alpha = 0.05))
  expect_error(mfcurve_stat_testing(data, test = "invalid", alpha = 0.05))
  expect_error(mfcurve_stat_testing(data, test = "mean", alpha = 1.5))
})
