
source("R/preprocessing.R")
source("R/stat_testing.R")
source("R/plotting.R")

# Simulated dataset
set.seed(123)
n <- 1000
df <- data.frame(
  wage = rnorm(n, mean = 15, sd = 5),
  race = sample(c("White", "Black", "Other"), n, replace = TRUE),
  south = sample(c("Yes", "No"), n, replace = TRUE),
  union = sample(c("Yes", "No"), n, replace = TRUE)
)

# Preprocessing
preprocessed_data <- mfcurve_preprocessing(
  data = df,
  outcome_var = "wage",
  factors = c("race", "south", "union")
)

# View preprocessed data
head(preprocessed_data)

# Statistical testing
stat_test_results <- mfcurve_stat_testing(
  data = preprocessed_data,
  test = "mean", # Compare against the grand mean
  alpha = 0.05   # Significance level
)

# View statistical results
head(stat_test_results)

# Plotting
plot <- mfcurve_plotting(
  stats = stat_test_results,
  factors = c("race", "south", "union"),
  outcome = "wage",
  alpha = 0.05,
  showTitle = TRUE
)

# Display the plot
plot
